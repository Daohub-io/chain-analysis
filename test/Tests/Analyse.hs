{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Analyse where

import Control.Exception

import Data.Attoparsec.ByteString
import Data.ByteString (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Base16 as B16
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty, (<>))
import qualified Data.Set as S
import Data.Text.Encoding
import Data.Either

import Numeric.Natural

import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Check.Stores
import OpCode.Exporter
import OpCode.Parser
import OpCode.Type
import Process
import OpCode.Utils
import CompileSolidity
import Models.HandWritten

import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import Tests.Utils

storeCheckerTests = TestLabel "Store Checker" $ TestList $
    [ TestLabel "Should Pass Empty Code" $ TestCase $ do
        assertBool "Empty bytecode should pass store checker" (checkStores [])
    , TestLabel "Should Pass Code Without SSTORE calls" $ TestList
        [ TestLabel "Trivial Example" $ TestCase $ do
            assertBool "Calls without should pass store checker" (checkStores [PUSH1 (pack [0x4]), POP])
        , TestLabel "\"Adder\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Adder.sol"
            code <- parseGoodExample bsDecoded
            assertBool "Calls without should pass store checker" (checkStores code)
        , TestLabel "\"Fib\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Fib.sol"
            code <- parseGoodExample bsDecoded
            assertBool "Calls without should pass store checker" (checkStores code)
        ]
    , TestLabel "Should Reject Code With unprotected SSTORE calls" $ TestList
        [ TestLabel "Trivial Example" $ TestCase $ do
            -- This is example code that consists of a single unprotected SSTORE
            -- call.
            let code =
                    [ PUSH1 (pack [0x4])
                    , PUSH1 (pack [0x0])
                    , SSTORE
                    ]
            assertBool "Calls with unprotected SSTORE should not pass store checker" (not $ checkStores code)
        , TestLabel "\"Storer\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsDecoded <- compileSolidityFileBin "test/Models/Storer.sol"
            code <- parseGoodExample bsDecoded
            assertBool "Calls with unprotected SSTORE should not pass store checker" (not $ checkStores code)
        ]
    , TestLabel "Should Pass Code With protected SSTORE calls" $ TestList
        [ TestLabel "Trivial Example" $ TestCase $ do
            -- This is example code that consists of a single protected SSTORE
            -- call.
            let
                lowerLimit = 0x0100000000000000000000000000000000000000000000000000000000000000
                upperLimit = 0x0200000000000000000000000000000000000000000000000000000000000000
                code =
                    [ PUSH32 $ integerToEVM256 lowerLimit -- lower limit
                    , DUP2 -- duplicate store address for comparison
                    , OpCode.Type.LT -- see if address is lower than the lower limit
                    , PUSH32 $ integerToEVM256 upperLimit -- upper limit
                    , DUP3 -- duplicate store address for comparison
                    , OpCode.Type.GT -- see if the store address is higher than the upper limit
                    , OR -- set top of stack to 1 if either is true
                    , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
                    , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
                    , SWAP1 -- put the value on top with the key underneath
                    , DUP2 -- put a copy of the key on top
                    , SSTORE -- perform the store
                    ]
            assertBool "Protected stored calls should pass store checker" (checkStores code)
            assertEqual "Protected store a calls should match the required capabilities"
                (Ranges $ S.fromList [(lowerLimit, upperLimit)])
                (getRequiredCapabilities code)
        , TestLabel "\"StoreProtectedInline\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single SSTORE call, with all of the
            -- necessary protection code entered in Solidity assembly.
            bsDecoded <- compileSolidityFileBin "test/Models/Protection/StorerProtectedInline.sol"
            code <- parseGoodExample bsDecoded
            assertBool "Protected stored calls should pass store checker" (checkStores code)
        ]
    ]

testJumps code =
    if not $ null $ checkStaticJumps code
        then assertFailure $ "Static jumps not ok " ++ show (checkStaticJumps code)
        else pure ()
