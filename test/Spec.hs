module Main where

import Data.Attoparsec.ByteString
import Data.ByteString (pack)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Base16
import Data.Maybe
import Data.Monoid (mempty)
import System.Environment


import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import OpCode.Exporter
import OpCode.Parser
import OpCode.Type
import JumpChecker

import Data.List

main = do
    putStrLn "Running tests..."
    defaultMain tests

mainWithOpts = do

    -- Test options can also be specified in the code. The TestOptions
    -- type is an instance of the Monoid type class, so the easiest way
    -- to get an empty set of options is with `mempty`.
    let empty_test_opts = mempty :: TestOptions

    -- We update the empty TestOptions with our desired values.
    let my_test_opts = empty_test_opts
            { topt_maximum_generated_tests = Just 1000
            , topt_timeout = Just $ Just (4000::Int)
            }

    -- Now we create an empty RunnerOptions in the same way, and add
    -- our TestOptions to it.
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts
            { ropt_test_options = Just my_test_opts
            }

    defaultMainWithOpts tests my_runner_opts

tests =
    [ testGroup "OpCodes" $ (hUnitTestToTests singleOpCodes)
    -- , testGroup "Conversion" $ (hUnitTestToTests conversionTests)
    , testProperty "Round-Trip Single OpCode" prop_anyValidOpCode_roundTrip
    ]

prop_anyValidOpCode_roundTrip :: OpCode -> Bool
prop_anyValidOpCode_roundTrip opCode =
    let bs = toByteString opCode
        parseResult = parse parseOpCode bs
    in case parseResult of
        Fail i contexts err -> False
        -- Partial _ ->
        Done i oc -> True

-- Parse each of the opcodes individually.
-- TODO: finish this list.
singleOpCodes = TestLabel "SingleOpCodes" $ TestList
    [ singleParseTest parseSTOP STOP (pack [0x00])
    , singleParseTest parseADD ADD (pack [0x01])
    , singleParseTest parseMUL MUL (pack [0x02])
    , singleParseTest parseSUB SUB (pack [0x03])
    , singleParseTest parseDIV DIV (pack [0x04])
    , singleParseTest parseSDIV SDIV (pack [0x05])
    , singleParseTest parseMOD MOD (pack [0x06])
    , singleParseTest parseSMOD SMOD (pack [0x07])
    , singleParseTest parseADDMOD ADDMOD (pack [0x08])
    , singleParseTest parseMULMOD MULMOD (pack [0x09])
    , singleParseTest parseEXP EXP (pack [0x0a])
    , singleParseTest parseSIGNEXTEND SIGNEXTEND (pack [0x0b])

    ]

singleParseTest parser target unparsed = TestLabel ("Parse " ++ show target ++ " OpCode") $ TestCase $ do
    case parseOnly (parser <* endOfInput) unparsed of
        Left err -> assertFailure $ "Opcodes should be parsed in full: " ++ err
        Right parsed -> assertEqual "parsed and target should be equal" parsed target
    -- For the opposite test, if input is MUL, use SUB, else use MUL
    let contraCase = if unparsed == pack [0x02] then pack [0x03] else pack [0x02]
    case parseOnly (parser <* endOfInput) contraCase of
        Left err -> pure ()
        Right parsed -> assertBool "parsed and target should not be equal" (parsed /= target)


-- Parse something that is not the STOP OpCode
parseSTOPTestNot = TestLabel "Parse STOP OpCode Not" $ TestCase $ do
    case parseOnly (parseSTOP <* endOfInput) (pack [0x01]) of
        Left _ -> pure ()
        Right oc -> assertFailure $ "No opcode should be parsed, but " ++ show oc ++ " was parsed"

testExampleContract = TestLabel "Parse Example Contract" $ TestCase $ do
    let (bs,_) = decode $ C8.pack "6060604052341561000f57600080fd5b60ba8061001d6000396000f300606060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b3415604e57600080fd5b606b60048080359060200190919080359060200190919050506081565b6040518082815260200191505060405180910390f35b60008183019050929150505600a165627a7a723058203691f76bc66e6d69f31bbfe2298197772c0c4c64b8eaefe9eec731a4e91fc1a50029"
    case parse (parseOpCodes <* endOfInput) bs of
        Fail i contexts err ->
            assertFailure $ "Opcodes should be parsed in full, remainder: " ++ show (encode i)
        Done i ocs -> do
            mapM_ print ocs
            pure ()

parserTests = TestLabel "OpCode Parser" $ TestList $
    [ TestLabel "Should Parse Empty Code" $ TestCase $ do
        undefined
    , TestLabel "Should All Single Valid OpCodes" $ TestCase $ do
        undefined
    , TestLabel "Should Reject Single Invalid OpCodes" $ TestCase $ do
        undefined
    , TestLabel "Should Parse Simple Hand-Written Samples" $ TestCase $ do
        undefined
    , TestLabel "Should Parse Compiled Examples" $ TestList $
        [ TestLabel "Should Parse \"Storer\"" $ TestCase $ do
            undefined
        , TestLabel "Should Parse \"Adder\"" $ TestCase $ do
            undefined
        ]
    ]

jumpCheckerTests = TestLabel "Jump Checker" $ TestList $
    [ TestLabel "Should Accept Empty Code" $ TestCase $ do
        undefined
    , TestLabel "Should Accept Code Without Jumps" $ TestCase $ do
        undefined
    , TestLabel "Should Accept Code With Any Invalid Jumps" $ TestCase $ do
        undefined
    , TestLabel "Should Accept Code With Only Valid Jumps" $ TestCase $ do
        undefined
    ]

-- preprocessorTests = TestLabel "Preprocessor" $ TestList $
--     [ TestLabel "Passthrough" $ TestList $
--         [ shouldRejectInvalidCode
--         , validCodeShouldRemainUnchanged
--         ]
--     , TestLabel "Append OpCodes" $ TestList $
--         [ theCodeShouldBeValid
--         , lengthOfCodeShouldIncrease
--         , theOpCodesShouldBeAppended
--         , theJumpsShouldBeValid
--         -- , theJumpsDestinationsShouldBeMaintained
--         ]
--     , TestLabel "Insert OpCodes" $ TestList $
--         [ theCodeShouldBeValid
--         , lengthOfCodeShouldIncrease
--         , theOpCodesShouldBeInserted
--         , theJumpsShouldBeValid
--         -- , theJumpsDestinationsShouldBeMaintained
--         ]
--     , TestLabel "Remove OpCodes" $ TestList $
--         [ theCodeShouldBeValid
--         , lengthOfCodeShouldDecrease
--         , theOpCodesShouldBeRemoved
--         , theJumpsShouldBeValid
--         -- , theJumpsDestinationsShouldBeMaintained
--         ]
--     , TestLabel "Insert and Remove Opcodes" $ TestList $
--         [ theCodeShouldBeValid
--         , lengthOfCodeShouldDecrease
--         , theOpCodesShouldBeRemoved
--         , theJumpsShouldBeValid
--         -- , theJumpsDestinationsShouldBeMaintained
--         ]
--     ]

