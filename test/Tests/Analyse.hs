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
import OpCode.StructureParser
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
        let checkPass = case checkStores [] of
                Left e -> error $ show e
                Right x -> x
        assertBool "Empty bytecode should pass store checker" checkPass
    , TestLabel "Should Pass Code Without SSTORE calls" $ TestList
        [ TestLabel "Trivial Example" $ TestCase $ do
            let checkPass = case checkStores [PUSH1 (pack [0x4]), POP] of
                    Left e -> error $ show e
                    Right x -> x
            assertBool "Calls without should pass store checker" checkPass
        , TestLabel "\"Adder\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Adder.sol"
            code <- parseGoodExample bsDecoded
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool "Calls without should pass store checker" checkPass
        , TestLabel "\"Fib\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Fib.sol"
            code <- parseGoodExample bsDecoded
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool "Calls without should pass store checker" checkPass
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
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "Calls with unprotected SSTORE should not pass store checker"
                (not checkPass)
        , TestLabel "\"Storer\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsDecoded <- compileSolidityFileBin "test/Models/Storer.sol"
            code <- parseGoodExample bsDecoded
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "Calls with unprotected SSTORE should not pass store checker"
                (not checkPass)
        , TestLabel "\"Storer\" getCapabilities" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsDecoded <- compileSolidityFileBin "test/Models/Storer.sol"
            code <- parseGoodExample bsDecoded
            let lowerLimit = 0x0100000000000000000000000000000000000000000000000000000000000000
                upperLimit = 0x0200000000000000000000000000000000000000000000000000000000000000
            let Right parsed =  fullStructuredParse code
            assertEqual
                "Calls with unprotected SSTORE should require Any writie cap"
                (Right $ Any)
                (getRequiredCapabilities code)
        ]
    , TestLabel "Should Pass Code With protected SSTORE calls" $ TestList
        [ TestLabel "Trivial Example" $ TestCase $ do
            -- This is example code that consists of a single protected SSTORE
            -- call.
            let lowerLimit = 0x0100000000000000000000000000000000000000000000000000000000000000
                upperLimit = 0x0200000000000000000000000000000000000000000000000000000000000000
                topic = keccak256Bytes "KERNEL_SSTORE"
            let code =
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

                    , PUSH1 (pack [0x60])
                    , MLOAD
                    , PUSH1 (pack [0x80])
                    , MLOAD

                    , ADDRESS
                    , PUSH1 (pack [0x60])
                    , MSTORE

                    , SWAP2
                    , PUSH1 (pack [0x80])
                    , MSTORE

                    , PUSH32 topic
                    , PUSH1 (pack [0x34])
                    , PUSH1 (pack [0x6c])

                    , LOG1

                    , PUSH1 (pack [0x60])
                    , MSTORE
                    , PUSH1 (pack [0x80])
                    , MSTORE
                    ]
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "Protected stored calls should pass store checker"
                checkPass
            assertEqual "Protected store a calls should match the required capabilities"
                (Right $ Ranges $ S.fromList [(lowerLimit, upperLimit)])
                (getRequiredCapabilities code)
        , TestLabel "\"StoreProtectedInline\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single SSTORE call, with all of the
            -- necessary protection code entered in Solidity assembly.
            bsDecoded <- compileSolidityFileBin "test/Models/Protection/StorerProtectedInline.sol"
            code <- parseGoodExample bsDecoded
            let checkPass = case checkStores code of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "Protected stored calls should pass store checker"
                (not checkPass)
        ]
    ]

testJumps code =
    if not $ null $ checkStaticJumps code
        then assertFailure $ "Static jumps not ok " ++ show (checkStaticJumps code)
        else pure ()
