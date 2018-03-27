module Check.Stores where

import OpCode.Type
import Data.ByteString (pack)

checkStores :: [OpCode] -> Bool
checkStores (a:b:c:d:e)
    | any isSSTORE [a,b,c,d] = False
    | otherwise = checkStores' (a:b:c:d:e)
checkStores cs = not $ hasSSTORE cs

checkStores' :: [OpCode] -> Bool
checkStores' (a:b:c:d:e:cs)
    | not (isSSTORE e) = checkStores' (b:c:d:e:cs)
    | otherwise = and
        [ a == PUSH1 (pack [0x64])
        , b == PUSH1 (pack [0x40])
        , c == MLOAD
        , d == MSTORE
        ]
checkStores' _ = True

isSSTORE :: OpCode -> Bool
isSSTORE SSTORE = True
isSSTORE _ = False

hasSSTORE :: [OpCode] -> Bool
hasSSTORE (SSTORE:cs) = True
hasSSTORE (_:cs) = hasSSTORE cs
hasSSTORE [] = False

-- protectedStoreSequence startLocation lowerLimit upperLimit =
--     [ PUSH32 storeProcEnd
--     , JUMP -- rudimentary storage protection
--     , JUMPDEST -- storeProc
--     , SWAP1
--     , PUSH32 lowerLimit
--     , DUP2
--     , LT
--     , PUSH32 upperLimit
--     , DUP3
--     , GT
--     , OR
--     , excAddress
--     , JUMPI
--     , SWAP1
--     , SWAP2
--     , SWAP1
--     , SSTORE
--     , JUMP
--     , JUMPDEST -- excAddress
--     , PUSH1 (pack [0x00]) -- revert start location
--     , PUSH1 (pack [0x00]) -- revert end location
--     , REVERT
--     , JUMPDEST -- storeProcEnd
--     ]
