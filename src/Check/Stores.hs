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