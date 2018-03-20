module Main where

import Data.Attoparsec.ByteString
import Data.ByteString (pack)
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
    -- , testGroup "Upload" $ hUnitTestToTests uploadTests
    -- , testGroup "Email" $ hUnitTestToTests emailTests
    -- , testGroup "TimingLogs" $ hUnitTestToTests timeLoggingTests
    -- , testProperty "leftOrRight Order"  prop_leftOrRight_order
    -- , testProperty "isConvex Start Point" prop_isConvex_startPoint
    -- , testProperty "isQuadConvex Order" prop_isQuadConvex_order
    ]

-- |Parse each of the opcodes individually.
singleOpCodes = TestLabel "SingleOpCodes" $ TestList
    [ parseSTOPTest
    , parseSTOPTestNot
    ]

parseSTOPTest = TestLabel "Parse STOP OpCode" $ TestCase $ do
    let res = parseOnly (parseSTOP <* endOfInput) (pack [0x00])
    case res of
        Right STOP -> pure ()
        _ -> error "FSD"
parseSTOPTestNot = TestLabel "Parse STOP OpCode Not" $ TestCase $ do
    let res = parseOnly (parseSTOP <* endOfInput) (pack [0x01])
    case res of
        Left _ -> pure ()
        Right _ -> error "Fds"
