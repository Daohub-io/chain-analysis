{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Transform where

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
import Check.JumpTable
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
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

import Tests.Analyse
import Tests.Utils
import Tests.Transform.AdderOnChainProtected
import Tests.Transform.AdderOnChainUnprotected
import Tests.Transform.StoreAndGetOnChainProtectedInBounds
import Tests.Transform.StoreAndGetOnChainProtectedOutOfBounds
import Tests.Transform.StoreAndGetOnChainUnprotectedInBounds

preprocessorTests = TestLabel "Preprocessor" $ TestList $
    -- [ TestLabel "Passthrough" $ TestList $
    --     [ TestLabel "Should Reject Invalid Code" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Leave Valid Code Unchanged" $ TestCase $ do
    --         undefined
    --     ]
    [ TestLabel "Storage Protection" $ TestList $
        [ TestLabel "Should Leave Code w/o Jumps or JumpDests Unchanged" $ TestCase $ do
            let code = [STOP, STOP, STOP]
            assertEqual "Code should remain unchanged" code (transformDeployed defaultCaps code)
        , TestLabel "Should Add a table for a single jump (no stores)" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    ]
                dests = map snd $ countCodes code
                expected = map Just [0,2,3,4]
            assertEqual "Jump dests should be correct" expected dests
        , TestLabel "Jump Table Consistency Without Store" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    ]
                defaultCaps = Capabilities
                    { caps_storageRange  = (0x0100000000000000000000000000000000000000000000000000000000000000,0x0200000000000000000000000000000000000000000000000000000000000000)
                    }
                tableWithProtectedStores = replaceVars $ appendJumpTable $ replaceJumps $ insertProtections defaultCaps $ countCodes code
                tableWithoutProtectedStores = replaceVars $ appendJumpTable $ replaceJumps $ countCodes code
            assertEqual "The added jump table should be the same whether storage protection is run or not" tableWithoutProtectedStores tableWithProtectedStores
        , TestLabel "Should add a table for a single jump (no stores)" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    ]
            case extractJumpTable $ transformDeployed defaultCaps code of
                Just entries -> do
                    assertEqual "The jump table should have a single entry" 1 (length entries)
                    let [(original, remapping)] = entries
                    assertEqual "The single jump table entry should be for 0x4" 0x4 original
                Nothing -> assertFailure "A jump table should be present"
        , TestLabel "Should add storage protection code (store only)" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , PUSH1 (pack [0x4])
                    , SSTORE
                    ]
                transformed = transformDeployed defaultCaps code
            case extractJumpTable transformed of
                Just entries -> assertFailure "A jump table should not be present"
                Nothing -> pure ()
            case getRequiredCapabilities transformed of
                Left e -> assertFailure ("Capability determination failed: " ++ show e)
                Right Any -> assertFailure "Require capability is Any, but should be restricted"
                Right (Ranges rs) -> assertEqual "Storage capability range should be as specified" (S.singleton $ caps_storageRange defaultCaps) (rs)
        , TestLabel "Should add storage protection code (store and jump)" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , PUSH1 (pack [0x4])
                    , SSTORE
                    , PUSH1 (pack [0x9])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    , PUSH1 (pack [0x4])
                    , POP
                    ]
                transformed = transformDeployed defaultCaps code
            case extractJumpTable transformed of
                Just entries -> do
                    assertEqual "The jump table should have a single entry" 1 (length entries)
                    let [(original, remapping)] = entries
                    assertEqual "The single jump table entry should be for 0x9" 0x9 original
                Nothing -> assertFailure "A jump table should be present"
            case getRequiredCapabilities transformed of
                Left e -> assertFailure ("Capability determination failed: " ++ show e)
                Right Any -> assertFailure "Require capability is Any, but should be restricted"
                Right (Ranges rs) -> assertEqual "Storage capability range should be as specified" (S.singleton $ caps_storageRange defaultCaps) (rs)
        , TestLabel "\"Storer\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsEncodedFull <- compileSolidityFileBinFull "test/Models/Storer.sol"
            bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/Storer.sol"
            let
                bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
                bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
                    in if remainder == B.empty then bytes else error (show remainder)
            code <- parseGoodExample bsDecodedFull
            let checkPassUntransformed = case (checkStores code) of
                    Left e -> error $ show e
                    Right x -> x
            assertBool "Calls with unprotected SSTORE should not pass store checker" (not checkPassUntransformed)
            -- after transformation it should pass store checker
            let checkPassTransformed = case (checkStores $ transform defaultCaps code) of
                    Left e -> error $ show e
                    Right x -> x
            assertBool "After transformation should pass store checker" checkPassTransformed
        , TestLabel "\"StorerWithAdd\"" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsEncodedFull <- compileSolidityFileBinFull "test/Models/StorerWithAdd.sol"
            bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/StorerWithAdd.sol"
            let
                bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
                bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
                    in if remainder == B.empty then bytes else error (show remainder)
            code <- parseGoodExample bsDecodedFull
            let checkPassUntransformed = case (checkStores code) of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "Calls with unprotected SSTORE should not pass store checker"
                (not checkPassUntransformed)
            -- after transformation it should pass store checker
            let checkPassTransformed = case (checkStores $ transform defaultCaps code) of
                    Left e -> error $ show e
                    Right x -> x
            assertBool
                "After transformation should pass store checker"
                checkPassTransformed
        , TestLabel "\"StorerWithAdd\" on chain" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            newContractAddress <- deployFromFile id "test/Models/StorerWithAdd.sol"
            (Right (res)) <- runWeb3 $ do
                accs <- accounts
                let sender = case accs of
                        [] -> error "No accounts available"
                        (a:_) -> a
                let details = Call {
                        callFrom = Just sender,
                        callTo = Just newContractAddress,
                        callGas = Nothing,
                        callGasPrice = Nothing,
                        callValue = Nothing,
                        callData = Just ((JsonAbi.methodId (DFunction "storeWithAdd" False
                            [ FunctionArg "a" "uint256"
                            , FunctionArg "b" "uint256"
                            ] (Just [FunctionArg "" "uint256"]))) <> "0000000000000000000000000000000000000000000000000000000000000045" <> "0000000000000000000000000000000000000000000000000000000000000001")
                    }

                theCall <- Eth.call details Latest
                pure (theCall)
            assertEqual "Result" "0x0000000000000000000000000000000000000000000000000000000000000046" res
        , Tests.Transform.AdderOnChainUnprotected.test
        , Tests.Transform.AdderOnChainProtected.test
        , Tests.Transform.StoreAndGetOnChainUnprotectedInBounds.test
        , Tests.Transform.StoreAndGetOnChainProtectedInBounds.test
        , Tests.Transform.StoreAndGetOnChainProtectedOutOfBounds.test
        , TestLabel "Trivial on chain" $ TestCase $ do
            let bytecode =
                    [ PUSH1 (pack [0x10])
                    , DUP1
                    , PUSH1 (pack [0x0f])
                    , PUSH1 (pack [0x00])
                    , CODECOPY
                    , PUSH1 (pack [0x00])
                    , RETURN
                    , STOP
                    , PUSH1 (pack [0x00])
                    , CALLDATALOAD
                    , SLOAD
                    , NOT
                    , PUSH1 (pack [0x09])
                    , JUMPI
                    , STOP
                    , PUSH1 (pack [0x20])
                    , CALLDATALOAD
                    , PUSH1 (pack [0x00])
                    , CALLDATALOAD
                    , SSTORE
                    ]

            let bsEncoded = B16.encode $ B.concat $ map toByteString bytecode
            (Right sender) <- runWeb3 $ do
                accs <- accounts
                case accs of
                        [] -> error "No accounts available"
                        (a:_) -> pure a
            (res, tx) <- deployContract sender bsEncoded
            newContractAddress <- getContractAddress tx

            (Right code) <- runWeb3 $ getCode newContractAddress Latest
            actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
            pure ()
        ]
    , TestLabel "Init Presence" $ TestList
        [ TestLabel "\"Ballot\"" $ TestCase $ do
            -- #Deployment
            -- Take the contract from the file and deploy it, returning the address.
            r <- Control.Exception.try $ deployFromFile (transform defaultCaps) "test/Models/Ballot.sol" :: IO (Either SomeException Address.Address)
            case r of
                Left _ -> pure ()
                Right _ -> assertFailure "Contract transformation should have failed due to unrecognised init"
        , TestLabel "\"BallotNoConstructor\"" $ TestCase $ do
            -- #Deployment
            -- Take the contract from the file and deploy it, returning the address.
            newContractAddress <- deployFromFile (transform defaultCaps) "test/Models/BallotNoConstructor.sol"
            print newContractAddress
        ]
    ]

