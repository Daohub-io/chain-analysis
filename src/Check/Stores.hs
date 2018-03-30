module Check.Stores where

import OpCode.Type
import OpCode.Utils
import Data.ByteString (pack)
import Numeric.Natural
import qualified Data.Set as S

data StorageRange = Any | Ranges (S.Set (Natural, Natural)) deriving (Eq, Show)

-- |Currently just gets address ranges
getRequiredCapabilities :: [OpCode] -> StorageRange
getRequiredCapabilities code = getRequiredCapabilities' (Ranges S.empty) [] code

getRequiredCapabilities'
    :: StorageRange -- ^Accumulates storage requirements
    -> [OpCode] -- ^Processed @OpCode@s
    -> [OpCode] -- ^Unprocessed @OpCode@s
    -> StorageRange
getRequiredCapabilities' Any _ _ = Any
getRequiredCapabilities' (Ranges rs) ps (SSTORE:cs) =
    let newRCaps = case isProtectedStore (take 10 $ reverse ps ++ [SSTORE]) of
            Nothing -> Any
            Just range -> Ranges (S.insert range rs)
    in getRequiredCapabilities' newRCaps (SSTORE:ps) cs
getRequiredCapabilities' rcaps ps (c:cs) = getRequiredCapabilities' rcaps (c:ps) cs
getRequiredCapabilities' rcaps _ _ = rcaps

checkStores :: [OpCode] -> Bool
checkStores (a:b:c:d:e:f:g:h:i:cs)
    | any isSSTORE [a,b,c,d,e,f,g,h,i] = False
    | otherwise = checkStores' (a:b:c:d:e:f:g:h:i:cs)
checkStores cs = not $ hasSSTORE cs

checkStores' :: [OpCode] -> Bool
checkStores' (a:b:c:d:e:f:g:h:i:j:cs)
    | not (isSSTORE j) = checkStores' (b:c:d:e:f:g:h:i:j:cs)
    | otherwise = and
        [ a == (PUSH32 $ integerToEVM256 0x0100000000000000000000000000000000000000000000000000000000000000) -- lower limit
        , b == DUP2 -- duplicate store address for comparison
        , c == OpCode.Type.LT -- see if address is lower than the lower limit
        , d == (PUSH32 $ integerToEVM256 0x0200000000000000000000000000000000000000000000000000000000000000) -- upper limit
        , e == DUP3 -- duplicate store address for comparison
        , f == OpCode.Type.GT -- see if the store address is higher than the upper limit
        , g == OR -- set top of stack to 1 if either is true
        , h == PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
        , i == JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
        , j == SSTORE -- perform the store
        ] && checkStores' (b:c:d:e:f:g:h:i:j:cs)
checkStores' _ = True

-- |Check whether a sequence of @OpCode@s constitutes a protected SSTORE. Must
-- include the SSTORE call. Returns the required range if it is.
isProtectedStore :: [OpCode] -> Maybe (Natural, Natural)
isProtectedStore (a:b:c:d:e:f:g:h:i:SSTORE:cs) =
    let isProtected =  and
            [ isPUSH32 a -- lower limit
            , b == DUP2 -- duplicate store address for comparison
            , c == OpCode.Type.LT -- see if address is lower than the lower limit
            , isPUSH32 d -- upper limit
            , e == DUP3 -- duplicate store address for comparison
            , f == OpCode.Type.GT -- see if the store address is higher than the upper limit
            , g == OR -- set top of stack to 1 if either is true
            , h == PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
            , i == JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
            ]
    in if isProtected
        then
            let PUSH32 llv = a
                PUSH32 ulv = d
                ll = evm256ToInteger llv
                ul = evm256ToInteger ulv
            in Just (ll, ul)
        else Nothing
isProtectedStore _ = Nothing


isPUSH32 (PUSH32 _) = True
isPUSH32 _ = False

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
