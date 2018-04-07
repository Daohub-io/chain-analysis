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
module Process where

import Data.ByteString (empty, pack)
import Data.ByteString.Base16 (encode)
import qualified Data.Map as Map
import OpCode.Type
import OpCode.Utils

import Numeric.Natural

defaultCaps = Capabilities
    { caps_storageRange  = (0x0100000000000000000000000000000000000000000000000000000000000000,0x0200000000000000000000000000000000000000000000000000000000000000)
    }

transform :: [OpCode] -> [OpCode]
transform opCodes = replaceVars $ appendJumpTable $ replaceJumps $ insertProtections defaultCaps $ countCodes opCodes

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
countVarOpCodesAcc (counted, i) [] = reverse counted

jumpDestinations :: [CountedOpCode] -> [Integer]
jumpDestinations codes = foldl func [] codes where
    func dests x = case x of
        (JUMPDEST, Just n) -> n : dests
        _ -> dests

-- presumes that result doesn't contain JUMP and JUMPDEST opcodes
-- |See specification docs in the BeakerOS repository (https://github.com/DaoLab/beakeros/docs/0.1v/store-protect.md).
protectCall
    :: Capabilities-- ^Highest permissable address
    -> OpCode -- ^The opcode being protected
    -> [OpCode]
protectCall caps SSTORE =
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
protectCall _ code = [code]

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

replaceJumps :: [CountedOpCode] -> [VarOpCode]
replaceJumps codes = (codes >>= (\ccode -> case ccode of
    j@(JUMP, Just _) -> [ PushVar JumpTableDest256, Counted j]
    j@(JUMPI, Just _) -> [ Counted (SWAP1, Nothing), PushVar JumpTableDest256, Counted j]
    _ -> [Counted ccode])
    )

-- |Returns a tuple of the jump destination of the jump table and the code.
appendJumpTable :: [VarOpCode] -> (Integer, Integer,  [VarOpCode])
appendJumpTable codes = (jumpTableDest, jumpDispatchDest, codes ++ table)
    where
        table = jumpTable (jumpDests codes)
        jumpTableDest = (+1) $ (\(_,i)->i) $ last $ countVarOpCodes codes
        jumpDispatchDest = jumpTableDest + (fromIntegral $ sum $ map nBytesVar table) - 4

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
-- TODO: implement
jumpTable :: Map.Map Integer Integer -> [VarOpCode]
-- jumpTable jumpDests = [ PushVar tailJumpDest ] ++ map PushVar jumpDests
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
jumpDests :: [VarOpCode] -> Map.Map Integer Integer
jumpDests codes = foldl f Map.empty reCounted
    where
        -- Recount to determine the new positions of codes
        reCounted = countVarOpCodes codes
        -- If we find a jump dest, add (k,v) (oldLocation, newLocation) to the
        -- map
        f m (Counted (JUMPDEST, (Just n)), i) = Map.insert n i m
        -- Otherwise do nothing.
        f m _ = m

replaceVars :: (Integer, Integer, [VarOpCode]) -> [OpCode]
replaceVars (jumpTableDest, jumpDispatchDest, varOpCodes) = map (\vCode -> case vCode of
    PushVar JumpTableDest256 -> PUSH32 (integerToEVM256 $ fromIntegral jumpTableDest)
    PushVar JDispatch256 -> PUSH32 (integerToEVM256 $ fromIntegral jumpDispatchDest)
    Counted (code, _) -> code
    ) varOpCodes
    -- where --TODO: catch exceptions properly!
    --     varMap = varMapping varOpCodes
