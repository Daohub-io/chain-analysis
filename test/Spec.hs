module Main where

import Data.Attoparsec.ByteString
import Data.ByteString (pack)
-- import qualified Data.ByteString.Char8 as C8 (pack)
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

import Lib

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
    -- , testProperty "leftOrRight Order"  prop_leftOrRight_order
    ]

-- Parse each of the opcodes individually.
singleOpCodes = TestLabel "SingleOpCodes" $ TestList
    [ parseSTOPTest
    , parseSTOPTestNot
    ]

parseSTOPTest = TestLabel "Parse STOP OpCode" $ TestCase $ do
    let res = parseOnly (parseSTOP <* endOfInput) (pack [0x00])
    case res of
        Right STOP -> pure ()
        _ -> assertFailure $ "STOP should be parsed, but was not"
-- Parse something that is not the STOP OpCode
parseSTOPTestNot = TestLabel "Parse STOP OpCode Not" $ TestCase $ do
    case parseOnly (parseSTOP <* endOfInput) (pack [0x01]) of
        Left _ -> pure ()
        Right oc -> assertFailure $ "No opcode should be parsed, but " ++ show oc ++ " was parsed"

testExampleContract = TestLabel "Parse Example Contract" $ TestCase $ do
    let (bs,_) = decode $ C8.pack "6060604052341561000f57600080fd5b60ba8061001d6000396000f300606060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b3415604e57600080fd5b606b60048080359060200190919080359060200190919050506081565b6040518082815260200191505060405180910390f35b60008183019050929150505600a165627a7a72305820208e94342b2b01f28a6a3e363d85bb930b900adbc78c5ac5c926c3c316c993840029"
    case parseOnly (parseOpCodes <* endOfInput) (pack [0x01]) of
        Left _ -> assertFailure $ "Opcodes should be parsed in fulle"
        Right ocs -> pure ()
