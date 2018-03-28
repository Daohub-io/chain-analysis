-- |General interface for bytecode transformers.
module Process where

import Data.ByteString (empty)
import Data.ByteString.Base16 (encode)
import qualified Data.Map as Map
import OpCode.Type

transform :: [OpCode] -> [OpCode]
transform opCodes = replaceVars $ replaceJumps $ insertProtections $ countCodes opCodes

type CountedOpCode = (OpCode, Maybe Integer)

countCodes :: [OpCode] -> [CountedOpCode]
countCodes = countCodesAcc ([], 0) 

countCodesAcc :: ([CountedOpCode], Integer) -> [OpCode] -> [CountedOpCode]
countCodesAcc (counted, i) (x:xs) = countCodesAcc ((x, Just nextValue) : counted, i + 1) xs
    where nextValue = case x of 
            PUSH1 _ -> i + 2
            PUSH2 _ -> i + 3
            PUSH3 _ -> i + 4
            PUSH4 _ -> i + 5
            PUSH5 _ -> i + 6
            PUSH6 _ -> i + 7
            PUSH7 _ -> i + 8
            PUSH8 _ -> i + 9
            PUSH9 _ -> i + 10
            PUSH10 _ -> i + 11
            PUSH11 _ -> i + 12
            PUSH12 _ -> i + 13
            PUSH13 _ -> i + 14
            PUSH14 _ -> i + 15
            PUSH15 _ -> i + 16
            PUSH16 _ -> i + 17
            PUSH17 _ -> i + 18
            PUSH18 _ -> i + 19
            PUSH19 _ -> i + 20
            PUSH20 _ -> i + 21
            PUSH21 _ -> i + 22
            PUSH22 _ -> i + 23
            PUSH23 _ -> i + 24
            PUSH24 _ -> i + 25
            PUSH25 _ -> i + 26
            PUSH26 _ -> i + 27
            PUSH27 _ -> i + 28
            PUSH28 _ -> i + 29
            PUSH29 _ -> i + 30
            PUSH30 _ -> i + 31
            PUSH31 _ -> i + 32
            PUSH32 _ -> i + 33
            _ -> i + 1 
countCodesAcc (counted, i) [] = counted

jumpDestinations :: [CountedOpCode] -> [Integer]
jumpDestinations codes = foldl func [] codes where
    func dests x = case x of
        (JUMPDEST, Just n) -> n : dests
        _ -> dests

-- presumes that result doesn't contain JUMP and JUMPDEST opcodes
-- TODO: move somewhere and implement
protectCall :: OpCode -> [OpCode]
-- protectCall SSTORE ...
protectCall code = [code] 

insertProtections :: [CountedOpCode] -> [CountedOpCode]
insertProtections codes = codes >>= (\ccode -> case ccode of
    (opCode, _) -> fmap (\c -> (c, Nothing)) (protectCall opCode))

data VarOpCode = Counted CountedOpCode | PushVar Integer deriving Show

replaceJumps :: [CountedOpCode] -> [VarOpCode] 
replaceJumps codes = (codes >>= (\ccode -> case ccode of 
    j@(JUMP, _) -> [ PushVar tailJumpDest, Counted j]
    _ -> [Counted ccode])) ++ jumpTail tailJumpDest (jumpDestinations codes) where
    tailJumpDest = -1

-- add tail like
-- pushVar tailJumpDest
-- pop = pop() 
-- if (pop == j1) PushVar j1; jump
-- ...
-- if (pop == jk) PushVar jk; jump
-- TODO: implement
jumpTail :: Integer -> [Integer] -> [VarOpCode]
jumpTail tailJumpDest jumpDests = [ PushVar tailJumpDest ] ++ map PushVar jumpDests

varMapping :: [VarOpCode] -> Map.Map Integer Integer
varMapping codes = Map.fromList (map varMap (zip countedCodes codes)) where
    countedCodes = countCodes (map (\c -> case c of
        Counted (oCode, _) -> oCode
        PushVar _ -> PUSH1 empty) codes) -- TODO: check PUSH1 or not 
    varMap x = case x of 
        ((_, Just i), Counted (_, Just j)) -> (i, j)
        _ -> (-2, -2) -- TODO: use Maybe!

replaceVars :: [VarOpCode] -> [OpCode]
replaceVars varOpCodes = map (\vCode -> case vCode of
    PushVar x -> case Map.lookup x varMap of
        Just y -> PUSH1 empty -- TODO replace with PUSH1 encoded y 
        Nothing -> undefined
    Counted (code, _) -> code) varOpCodes where --TODO: catch exceptions properly! 
        varMap = varMapping varOpCodes
