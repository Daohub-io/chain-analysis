module Check.Stores where

import OpCode.StructureParser
import OpCode.Type
import OpCode.Utils
import Data.ByteString (pack)
import Numeric.Natural
import qualified Data.Set as S

import Data.List (find)

import Process (countCodes)

import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Prim as Parsec

data StorageRange = Any | Ranges (S.Set (Natural, Natural)) deriving (Eq, Show)

-- |Currently just gets address ranges
getRequiredCapabilities :: [OpCode] -> Either ParseError StorageRange
getRequiredCapabilities code = do
    parsed <- fullStructuredParse code
    pure $ getRequiredCapabilities' (Ranges S.empty) parsed

getRequiredCapabilities'
    :: StorageRange -- ^Accumulates storage requirements
    -> [StructuredCode] -- ^Unprocessed @StructureCode@s
    -> StorageRange
getRequiredCapabilities' Any _ = Any
getRequiredCapabilities' (Ranges rs) ((ProtectedStoreCall range):cs) =
    let newRCaps = Ranges (S.insert range rs)
    in getRequiredCapabilities' newRCaps cs
getRequiredCapabilities' rcaps (UnprotectedStoreCall:cs) = getRequiredCapabilities' Any cs
getRequiredCapabilities' rcaps (_:cs) = getRequiredCapabilities' rcaps cs
getRequiredCapabilities' rcaps [] = rcaps

-- |This only checks tht protection is in place, not that it is the correct
-- protection.
checkStores :: [OpCode] -> Either ParseError Bool
checkStores code = do
    parsed <- fullStructuredParse code
    pure $ all (not . isUnprotectedStore) parsed

isUnprotectedStore UnprotectedStoreCall = True
isUnprotectedStore _ = False

-- |Check whether a sequence of @OpCode@s constitutes a protected SSTORE. Must
-- include the SSTORE call. Returns the required range if it is.
isProtectedStore :: [OpCode] -> Maybe (Natural, Natural)
isProtectedStore codes =
    case Parsec.parse parseLoggedAndProtectedSSTORE "isProtectedStore" codes of
            Right range -> Just range
            _ -> Nothing

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

-- |Checks all the statically resolvable jumps, and ensures they lie on
-- JUMPDESTs. Returns a list of problematic OpCodes
checkStaticJumps :: [OpCode] -> [(Int, FindResult)]
checkStaticJumps opcodes = checkStaticJumps' [] (countCodes opcodes) (countCodes opcodes)

checkStaticJumps' errs opcodes ((o1,c1):(o2,c2):os)
    | (isJUMP o2 || isJUMPI o2) && (isPush o1) =
        let pushVal = getPushVal o1
            errs' = case findCount (fromIntegral pushVal) opcodes of
                Found (JUMPDEST, _) -> errs
                ib@(Found a) -> ((fromIntegral pushVal),(FoundButWrong a)):errs
                ib@TooHigh -> ((fromIntegral pushVal),ib):errs -- error $ "Could not find index " ++ show pushVal ++ " at " ++ show c2 ++ ", last value " ++ show (last opcodes)
                ib@(InBetween a b) -> ((fromIntegral pushVal),ib):errs
        in  checkStaticJumps' errs' opcodes ((o2,c2):os)
    | otherwise = checkStaticJumps' errs opcodes ((o2,c2):os)
checkStaticJumps' errs _ [_] = errs
checkStaticJumps' errs _ [] = errs

data FindResult
    = Found (OpCode, Maybe Integer)
    | FoundButWrong (OpCode, Maybe Integer)
    | InBetween (OpCode, Maybe Integer) (OpCode, Maybe Integer)
    | TooHigh deriving (Show, Eq)
findCount :: Integer -> [(OpCode, Maybe Integer)] -> FindResult
findCount c (o1@(_,Just c1):o2@(_,Just c2):os)
    | c == c1 = Found o1
    | c == c2 = Found o2
    | c > c1 && c < c2 = InBetween o1 o2
    | c < c1 = error "should not occur"
    | otherwise = findCount c (o2:os)
findCount c [_] = TooHigh

isJUMP JUMP = True
isJUMP _ = False

isJUMPI JUMPI = True
isJUMPI _ = False
