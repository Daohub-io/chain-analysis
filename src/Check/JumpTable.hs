module Check.JumpTable where

import OpCode.Type
import OpCode.Utils
import Data.ByteString (pack)
import Numeric.Natural
import qualified Data.Set as S

import Data.List (find)

import Process (countCodes)

extractJumpTable :: [OpCode] -> Maybe [(Natural, Natural)]
extractJumpTable opcodes@(o:rem) = case takeJumpTable opcodes of
    Just x -> Just x
    Nothing -> extractJumpTable rem
extractJumpTable [] = Nothing

takeJumpTable :: [OpCode] -> Maybe [(Natural, Natural)]
-- Must start with a JUMPDEST
takeJumpTable (JUMPDEST:opcodes) =
    let (entries, rem1) = takeJumpTableEntries opcodes
        (dispatch, rem2) = takeJumpDispatch rem1
    in case (entries, dispatch, rem2) of
        (entries, Just dispatch, _) -> Just entries
        _ -> Nothing
takeJumpTable _ = Nothing

takeJumpTableEntries :: [OpCode] -> ([(Natural, Natural)], [OpCode])
takeJumpTableEntries opcodes = takeJumpTableEntries' [] opcodes

takeJumpTableEntries' :: [(Natural, Natural)] -> [OpCode] -> ([(Natural, Natural)], [OpCode])
takeJumpTableEntries' acc opcodes = case takeJumpTableEntry opcodes of
    (Just entry, rem) -> takeJumpTableEntries'(entry:acc) rem
    (Nothing, rem) -> (acc, rem)

takeJumpTableEntry :: [OpCode] -> (Maybe (Natural, Natural), [OpCode])
takeJumpTableEntry opcodes@(a:b:c:d:e:f:g:h:rem) =
    let isTableEntry = and
            [ a == DUP1
            , isPush b
            , c == OpCode.Type.EQ
            , isPush d
            , e == SWAP1
            , isPush f
            , g == JUMPI
            , h == POP
            ]
    in if isTableEntry
            then (Just (getPushVal b, getPushVal d), rem)
            else (Nothing, opcodes)
takeJumpTableEntry opcodes = (Nothing, opcodes)

takeJumpDispatch :: [OpCode] -> (Maybe (), [OpCode])
takeJumpDispatch (a:b:c:d:opcodes) =
    let isDispatch = and
            [ a == JUMPDEST
            , b == SWAP1
            , c == POP
            , d == JUMP
            ]
    in if isDispatch
            then (Just (), opcodes)
            else (Nothing, opcodes)
    -- | Map.null jumpDests = []
    -- | otherwise = [Counted (JUMPDEST, Nothing)] ++ (concat $ Map.elems $ Map.mapWithKey mkJTEntry jumpDests) ++ jumpDispatch
    -- where
    --     mkJTEntry old new =
    --         [ Counted (DUP1, Nothing)
    --         , Counted (PUSH32 (integerToEVM256 $ fromIntegral old), Nothing)
    --         , Counted (OpCode.Type.EQ, Nothing)
    --         , Counted (PUSH32 (integerToEVM256 $ fromIntegral new), Nothing)
    --         , Counted (SWAP1, Nothing)
    --         , PushVar JDispatch256
    --         , Counted (JUMPI, Nothing)
    --         , Counted (POP, Nothing)
    --         ]
    --     jumpDispatch =
    --         [ Counted (JUMPDEST, Nothing)
    --         , Counted (SWAP1, Nothing)
    --         , Counted (POP, Nothing)
    --         , Counted (JUMP, Nothing)
    --         ]