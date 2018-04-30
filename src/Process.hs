-- |General interface for bytecode transformers.
--
-- Code can be in a number of states
--
--   1. @[OpCode]@ - A list of opcodes as parsed by the parser, fully static.
--   2. @[CountedOpCode]@ - The same list of opcodes as in #1, but each @OpCode@
--      is given an index of its starting location, beginning from one (this may
--      need to be zero). In this state, opcodes can be inserted, but will carry
--      an index of Nothing.
--   3. @[VarOpCode]@ - A list of opcodes that each might be either a normal
--      @CountedOpCode@ (@Just n@, or @Nothing@ as an index) or a @PushVar@ with
--      an integer indicating a value that will need to be inserted later (such
--      as the location of the jump table). These insertions are made by
--      @replaceVars@.
{-# LANGUAGE OverloadedStrings #-}
module Process where

import Data.ByteString (empty, pack)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode)
import qualified Data.Map as Map
import OpCode.Type
import OpCode.Utils

import Numeric.Natural

import Debug.Trace

-- |These are the default capabilities used for testing.
defaultCaps :: Capabilities
defaultCaps = Capabilities
    { caps_storageRange  = (0x0100000000000000000000000000000000000000000000000000000000000000,0x0200000000000000000000000000000000000000000000000000000000000000)
    }

-- |Transform the opcodes by applying protections that satisfy the
-- @Capabilities@.
transform :: Capabilities -> [OpCode] -> [OpCode]
transform caps opCodes = replaceCodeCopy $ replaceVars $ appendJumpTable $ replaceJumps $ insertProtections caps $ countCodes opCodes

-- |Transform the opcodes by applying protections that satisfy the
-- @Capabilities@, ignoring the need for init code.
transformDeployed :: Capabilities -> [OpCode] -> [OpCode]
transformDeployed caps opCodes = replaceVars $ appendJumpTable $ replaceJumps $ insertProtections caps $ countCodes opCodes


midTransform caps opCodes = (\(a,b,c)->(a,b,countVarOpCodes c)) $ appendJumpTable $ replaceJumps $ insertProtections caps $ countCodes opCodes
type CountedOpCode = (OpCode, Maybe Integer)

-- |Check if a list of counted @OpCode@s is strictly monotonically increasing.
isMonotonic :: [CountedOpCode] -> Bool
isMonotonic opCodes = f Nothing opCodes
    where
        f Nothing ((_, currentIndex):xs) = f currentIndex xs
        f (Just lastIndex) ((_, Just currentIndex):xs)
            | currentIndex > lastIndex = f (Just currentIndex) xs
            | otherwise = False
        f l ((_, Nothing):xs) = f l xs
        f _ [] = True

countCodes :: [OpCode] -> [CountedOpCode]
countCodes = countCodesAcc ([], 0)

countCodesAcc :: ([CountedOpCode], Integer) -> [OpCode] -> [CountedOpCode]
countCodesAcc (counted, i) (x:xs) = countCodesAcc ((x, Just i) : counted, nextValue) xs
    where
        nextValue = i + (nBytes x)
countCodesAcc (counted, i) [] = reverse counted

countVarOpCodes :: [VarOpCode] -> [(VarOpCode, Integer)]
countVarOpCodes = countVarOpCodesAcc ([], 0)

countVarOpCodesAcc :: ([(VarOpCode, Integer)], Integer) -> [VarOpCode] -> [(VarOpCode, Integer)]
countVarOpCodesAcc (counted, i) (x:xs) = countVarOpCodesAcc ((x, i) : counted, nextValue) xs
    where
        nextValue = case x of
            Counted (x,_) -> i + (nBytes x)
            PushVar JumpTableDest256 -> i + 1 + 32
            PushVar JDispatch256 -> i + 1 + 32
countVarOpCodesAcc (counted, i) [] = reverse counted

jumpDestinations :: [CountedOpCode] -> [Integer]
jumpDestinations codes = foldl func [] codes where
    func dests x = case x of
        (JUMPDEST, Just n) -> n : dests
        _ -> dests

-- presumes that result doesn't contain JUMP and JUMPDEST opcodes
-- |See specification docs in the BeakerOS repository (https://github.com/DaoLab/beakeros/docs/0.1v/store-protect.md).
protectCall
    :: Capabilities-- ^Permissable address ranges
    -> OpCode -- ^The opcode being protected
    -> [OpCode]
-- protectCall caps SSTORE = protectStoreCall caps
protectCall caps SSTORE =
    (protectStoreCallLeaveKey caps)
    ++ logStoreCall
protectCall _ code = [code]

protectStoreCall :: Capabilities -> [OpCode]
protectStoreCall caps =
    [ PUSH32 $ integerToEVM256 ll -- lower limit
    , DUP2 -- duplicate store address for comparison
    , OpCode.Type.LT -- see if address is lower than the lower limit
    , PUSH32 $ integerToEVM256 ul -- upper limit
    , DUP3 -- duplicate store address for comparison
    , OpCode.Type.GT -- see if the store address is higher than the upper limit
    , OR -- set top of stack to 1 if either is true
    , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
    , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
    , SSTORE -- perform the store
    ]
    where
        (ll,ul) = caps_storageRange caps

-- |Like @protectStoreCall@ but leaves the storage key on top of the stack.
-- Takes one, leaves one (the same value) if successful, otherwise performs an
-- invalid jump.
protectStoreCallLeaveKey :: Capabilities -> [OpCode]
protectStoreCallLeaveKey caps =
    [ PUSH32 $ integerToEVM256 ll -- lower limit
    , DUP2 -- duplicate store address for comparison
    , OpCode.Type.LT -- see if address is lower than the lower limit
    , PUSH32 $ integerToEVM256 ul -- upper limit
    , DUP3 -- duplicate store address for comparison
    , OpCode.Type.GT -- see if the store address is higher than the upper limit
    , OR -- set top of stack to 1 if either is true
    , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
    , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
    , SWAP1 -- put the value on top with the key underneath
    , DUP2 -- put a copy of the key on top
    , SSTORE -- perform the store
    ]
    where
        (ll,ul) = caps_storageRange caps

-- |logCall assumes the storage key is at the top of the stack. Takes one,
-- leaves none. See specification docs
-- (https://github.com/DaoLab/beaker-preprocessor/docs/store-call-logging.md).
logStoreCall :: [OpCode]
logStoreCall =
    -- Load the original values of our memory buffer onto the stack.
    [ PUSH1 (pack [0x60])
    , MLOAD
    , PUSH1 (pack [0x80])
    , MLOAD

    -- Load the contract address onto the stack, then store it at memory
    -- location 0x60.
    , ADDRESS
    , PUSH1 (pack [0x60])
    , MSTORE

    -- Take the storage key from the stack and store it at 0x80. Note that it is
    -- in the 3rd position (beneath the two original memory values we just
    -- loaded). Therefore we must swap it to the top of the stack. This has a
    -- side effect in that it reverses the order of the two original memory
    -- values. Rather than swap them back, we simply account for that later.
    , SWAP2
    , PUSH1 (pack [0x80])
    , MSTORE

    -- Push the topic to which we publish to the stack. (NB: this is not defined
    -- here).
    , PUSH32 topic
    , PUSH1 (pack [0x34])
    , PUSH1 (pack [0x6c])

    -- Perform the LOG.
    , LOG1

    -- Restore the original memory values. Remember that the order of these is
    -- reversed by the SWAP2 used above, there we call MSTORE in the same order
    -- we called MLOAD.
    , PUSH1 (pack [0x60])
    , MSTORE
    , PUSH1 (pack [0x80])
    , MSTORE
    ]
    where
        topic = keccak256Bytes "KERNEL_SSTORE"

data Capabilities = Capabilities
    { caps_storageRange :: (Natural, Natural)
    }

insertProtections :: Capabilities -> [CountedOpCode] -> [CountedOpCode]
insertProtections caps codes = codes >>= (\ccode -> case ccode of
    (SSTORE, _) -> fmap (\c -> (c, Nothing)) (protectCall caps SSTORE)
    (opCode, n) -> fmap (\c -> (c, n)) (protectCall caps opCode)
    )

data VarOpCode = Counted CountedOpCode | PushVar PushVarVal deriving (Eq, Show)
data PushVarVal = JumpTableDest256 | JDispatch256 deriving (Eq, Show)

-- |TODO: this is a temporary proof of concept and is hardcoded to Solidity.
--
-- TODO: should use the structured code parser
--
-- TODO: currently this is optional but it should be enforced.
--
-- CODECOPY takes a length and a
-- byte offset from which to start copying code into memory. In order for a
-- contract to be created, bytecode is sent in a transaction. This bytecode is
-- then run *once*. This run should return a new bytecode, which is the bytecode
-- which forms the contract. To return code we need to use the RETURN opcode.
-- The return value is a section of memory indicated by a start offset an a
-- length, which are passed to the return opcode. RETURN is called with the
-- stack as follows.
--
-- @
--   TOP: 1. start_offset: the memory offset of the start of the return value.
--        2. length: the length of the return value in bytes
-- @
--
-- However, this requires that the final bytecode reside in memory. Therefore,
-- the code must be copied into memory. This is done via the CODECOPY opcode,
-- which takes a starting offset into the code, and a length, and copies that
-- length of code into a specified memory location. CODECOPY is called with the
-- stack as follows:
--
-- @
--   TOP: 1. start_mem_offest: The memory offset to start copying code to
--        2. code_start_offset: The bytecode offset to start copying code from
--        3. code_length: The length of code in bytes to be copied.
-- @
--
-- This only replaces the first instance in the code.

replaceCodeCopy :: [OpCode] -> [OpCode]
replaceCodeCopy code = replaceCodeCopy' lastByte [] code
    where
        lastByte = sum $ map nBytes code

replaceCodeCopy' lastByte acc (REVERT:JUMPDEST:(PUSH2 lengthbs):DUP1:(PUSH2 startbs):(PUSH1 memstartbs1):CODECOPY:(PUSH1 memstartbs2):RETURN:cs)
    = (reverse newAcc) ++ cs
    where
        length = (fromIntegral lastByte) - (fromIntegral $ evm256ToInteger startbs)
        start = startbs
        newAcc = (RETURN:(PUSH1 memstartbs2):CODECOPY:(PUSH1 memstartbs1):(PUSH2 start):DUP1:(PUSH2 (integerToEVM256 length)):JUMPDEST:REVERT:acc)

replaceCodeCopy' lastByte acc (REVERT:JUMPDEST:(PUSH1 lengthbs):DUP1:(PUSH2 startbs):(PUSH1 memstartbs1):CODECOPY:(PUSH1 memstartbs2):RETURN:cs)
    = (reverse newAcc) ++ cs
    where
        length = (fromIntegral (lastByte)) - (fromIntegral $ evm256ToInteger startbs)
        start = integerToEVM256 ((evm256ToInteger startbs) + 1)
        newAcc = (RETURN:(PUSH1 memstartbs2):CODECOPY:(PUSH1 memstartbs1):(PUSH2 start):DUP1:(PUSH2 (integerToEVM256 length)):JUMPDEST:REVERT:acc)

replaceCodeCopy' lastByte acc (c:cs) = replaceCodeCopy' lastByte (c:acc) cs
replaceCodeCopy' _ acc [] = error "no recognised init code"

replaceJumps :: [CountedOpCode] -> [VarOpCode]
replaceJumps codes = (codes >>= (\ccode -> case ccode of
    j@(JUMP, Just _) -> [ PushVar JumpTableDest256, Counted j]
    -- we don't want to do this to the first jump, so if it is in position 10,
    -- do nothing.
    -- TODO: this is a hack to match solidity init code and should be fixed.
    j@(JUMPI, Just 10) -> [ Counted j]
    j@(JUMPI, Just _) -> [ Counted (SWAP1, Nothing), PushVar JumpTableDest256, Counted j, Counted (POP, Nothing)]
    _ -> [Counted ccode])
    )

-- |Returns a tuple of the jump destination of the jump table and the code.
appendJumpTable :: [VarOpCode] -> (Integer, Integer,  [VarOpCode])
appendJumpTable codes = (jumpTableDest, jumpDispatchDest, codes ++ table)
    where
        -- adjust table location based on CODECOPY arguments
        codeOffset = case findCodeOffset codes of
            Just x -> -1*(fromIntegral x)
            Nothing -> 0
        table = jumpTable (jumpDests codeOffset codes)
        jumpTableDest = (\x->x+codeOffset) $ (+1) $ (\(_,i)->i) $ last $ countVarOpCodes codes
        -- jumpDispatchDest does not need the code offest as it's already
        -- considered in jumpTableDest.
        jumpDispatchDest = jumpTableDest + (fromIntegral $ sum $ map nBytesVar table) - 4

findCodeOffset :: [VarOpCode] -> Maybe Natural
findCodeOffset ((Counted ((PUSH2 startbs), _)):(Counted (PUSH1 memstartbs1, _)):(Counted (CODECOPY,_)):cs) = Just $ evm256ToInteger startbs
findCodeOffset (c:cs) = findCodeOffset cs
findCodeOffset [] = Nothing

-- -- |Returns a tuple of the jump destination of the jump table and the code.
-- appendJumpTableAsm :: [VarOpCode] -> [AsmOpCode]
-- appendJumpTableAsm codes = asmcodes ++ table
--     where
--         asmcodes = map f codes
--         f Counted (opcode, count) =
--         f PushVar PushVarVal
--         table = jumpTable (jumpDests codes)
--         jumpTableDest = (+1) $ (\(_,i)->i) $ last $ countVarOpCodes codes
--         jumpDispatchDest = jumpTableDest + (fromIntegral $ sum $ map nBytesVar table) - 1 - 4

nBytesVar (Counted (x,_)) = nBytes x
nBytesVar (PushVar x) = 1 + 32
-- add tail like
-- pushVar tailJumpDest
-- pop = pop()
-- if (pop == j1) PushVar j1; jump
-- ...
-- if (pop == jk) PushVar jk; jump
jumpTable :: Map.Map Integer Integer -> [VarOpCode]
jumpTable jumpDests
    | Map.null jumpDests = []
    | otherwise = [Counted (JUMPDEST, Nothing)] ++ (concat $ Map.elems $ Map.mapWithKey mkJTEntry jumpDests) ++ jumpDispatch
    where
        mkJTEntry old new =
            [ Counted (DUP1, Nothing)
            , Counted (PUSH32 (integerToEVM256 $ fromIntegral old), Nothing)
            , Counted (OpCode.Type.EQ, Nothing)
            , Counted (PUSH32 (integerToEVM256 $ fromIntegral new), Nothing)
            , Counted (SWAP1, Nothing)
            , PushVar JDispatch256
            , Counted (JUMPI, Nothing)
            , Counted (POP, Nothing)
            ]
        jumpDispatch =
            [ Counted (JUMPDEST, Nothing)
            , Counted (SWAP1, Nothing)
            , Counted (POP, Nothing)
            , Counted (JUMP, Nothing)
            ]

-- |Find a mapping from all jump destinations to their new destinations. First,
-- recount the byte locations to determine the new indices, then fold these into
-- a map.
jumpDests :: Integer -> [VarOpCode] -> Map.Map Integer Integer
jumpDests codeOffset codes = foldl f Map.empty reCounted
    where
        -- Recount to determine the new positions of codes
        reCounted = countVarOpCodes codes
        -- If we find a jump dest, add (k,v) (oldLocation, newLocation) to the
        -- map
        f m (Counted (JUMPDEST, (Just n)), i) = -- Map.insert n i m
            if n < (-1)*(codeOffset) || i < (-1)*(codeOffset)
                then m
                else Map.insert (n+codeOffset) (i+codeOffset) m
        -- Otherwise do nothing.
        f m _ = m

replaceVars :: (Integer, Integer, [VarOpCode]) -> [OpCode]
replaceVars (jumpTableDest, jumpDispatchDest, varOpCodes) = map (\vCode -> case vCode of
    PushVar JumpTableDest256 -> PUSH32 (integerToEVM256 $ fromIntegral jumpTableDest)
    PushVar JDispatch256 -> PUSH32 (integerToEVM256 $ fromIntegral jumpDispatchDest)
    Counted (code, _) -> code
    ) varOpCodes
