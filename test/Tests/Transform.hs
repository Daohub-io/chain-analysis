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

import Tests.Analyse
import Tests.Utils

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
            assertEqual "Code should remain unchanged" code (transform defaultCaps code)
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
        -- , TestLabel "Should Add a table for a single jump (no stores)" $ TestCase $ do
        --     let code =
        --             [ PUSH1 (pack [0x4])
        --             , JUMP
        --             , STOP
        --             , JUMPDEST
        --             ]
        --         defaultCaps = Capabilities
        --             { caps_storageRange  = (0x0100000000000000000000000000000000000000000000000000000000000000,0x0200000000000000000000000000000000000000000000000000000000000000)
        --             }
        --         table = jumpTable $ jumpDests $ replaceJumps $ insertProtections defaultCaps $ countCodes code
        --         table2 = jumpTable $ jumpDests $ replaceJumps $ countCodes code
        --         expected = (5,[Counted (STOP, Nothing)])
        --     assertEqual "Table should be correct" table2 table
        , TestLabel "Should Add a table for a single jump (no stores)" $ TestCase $ do
            let code =
                    [ PUSH1 (pack [0x4])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    ]
                expected =
                    [ PUSH1 (pack [0x4])
                    , PUSH32 (pack [38])
                    , JUMP
                    , STOP
                    , JUMPDEST
                    ] ++
                    [ JUMPDEST ] ++
                    [ DUP1
                    , PUSH32 (integerToEVM256 $ fromIntegral 4)
                    , OpCode.Type.EQ
                    , PUSH32 (integerToEVM256 $ fromIntegral 37)
                    , SWAP1
                    , PUSH32 (integerToEVM256 $ fromIntegral 143)
                    , JUMPI
                    , POP
                    ] ++
                    [ JUMPDEST
                    , SWAP1
                    , POP
                    , JUMP
                    ]
            assertEqual "Table should be added" expected (transform defaultCaps code)
        -- , TestLabel "Should insert additional opcodes to code with SSTORE (no jump)" $ TestCase $ do
        --     let code =
        --             [ PUSH1 (pack [0x4])
        --             , PUSH1 (pack [0x4])
        --             , SSTORE
        --             ]
        --         expected =
        --             [ PUSH1 (pack [0x4])
        --             , PUSH1 (pack [0x4])
        --             , PUSH32 $ integerToEVM256 0x0100000000000000000000000000000000000000000000000000000000000000 -- lower limit
        --             , DUP2 -- duplicate store address for comparison
        --             , OpCode.Type.LT -- see if address is lower than the lower limit
        --             , PUSH32 $ integerToEVM256 0x0200000000000000000000000000000000000000000000000000000000000000 -- upper limit
        --             , DUP3 -- duplicate store address for comparison
        --             , OpCode.Type.GT -- see if the store address is higher than the upper limit
        --             , OR -- set top of stack to 1 if either is true
        --             , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
        --             , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
        --             , SSTORE -- perform the store
        --             ]
        --         transformed = transform defaultCaps code
        --     assertEqual "The inserted code should be as expected" expected transformed
        -- , TestLabel "Should insert additional opcodes to code with SSTORE (plus a jump)" $ TestCase $ do
        --     let code =
        --             [ PUSH1 (pack [0x4])
        --             , PUSH1 (pack [0x4])
        --             , SSTORE
        --             , PUSH1 (pack [0x9])
        --             , JUMP
        --             , STOP
        --             , JUMPDEST
        --             , PUSH1 (pack [0x4])
        --             , POP
        --             ]
        --         expected =
        --             [ PUSH1 (pack [0x4])
        --             , PUSH1 (pack [0x4])
        --             , PUSH32 $ integerToEVM256 0x0100000000000000000000000000000000000000000000000000000000000000 -- lower limit
        --             , DUP2 -- duplicate store address for comparison
        --             , OpCode.Type.LT -- see if address is lower than the lower limit
        --             , PUSH32 $ integerToEVM256 0x0200000000000000000000000000000000000000000000000000000000000000 -- upper limit
        --             , DUP3 -- duplicate store address for comparison
        --             , OpCode.Type.GT -- see if the store address is higher than the upper limit
        --             , OR -- set top of stack to 1 if either is true
        --             , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
        --             , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
        --             , SSTORE -- perform the store
        --             , PUSH1 (pack [0x9])
        --             , PUSH32 (pack [119])
        --             , JUMP
        --             , STOP
        --             , JUMPDEST
        --             , PUSH1 (pack [0x4])
        --             , POP
        --             ] ++
        --             [ JUMPDEST ] ++
        --             [ DUP1
        --             , PUSH32 (integerToEVM256 $ fromIntegral 9)
        --             , OpCode.Type.EQ
        --             , PUSH32 (integerToEVM256 $ fromIntegral 115)
        --             , SWAP1
        --             , PUSH32 (integerToEVM256 $ fromIntegral 223)
        --             , JUMPI
        --             ] ++
        --             [ JUMPDEST
        --             , SWAP1
        --             , POP
        --             , JUMP
        --             ]
        --         transformed = transform defaultCaps code
        --     assertEqual "The inserted code should be as expected" (countCodes expected) (countCodes transformed)
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
            assertBool "Calls with unprotected SSTORE should not pass store checker" (not $ checkStores code)
            -- after transformation it should pass store checker
            assertBool "After transformation should pass store checker" (checkStores $ transform defaultCaps code)
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
            assertBool "Calls with unprotected SSTORE should not pass store checker" (not $ checkStores code)
            -- after transformation it should pass store checker
            assertBool "After transformation should pass store checker" (checkStores $ transform defaultCaps code)
        , TestLabel "\"StorerWithAdd\" on chain" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            newContractAddress <- deployFromFile id "test/Models/StorerWithAdd.sol"
            (Right (res)) <- runWeb3 $ do
                sender <- fmap (!! 1) accounts
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
        , adderProtectedOnChain
        , storeAndGetOnChainUnprotected
        , storeAndGetOnChainProtected
        , storeAndGetOnChainProtectedOutOfBounds
        , adderOnChainUntransformed
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
            (Right availableAccounts) <- runWeb3 accounts
            let sender = availableAccounts !! 1
            (res, tx) <- deployContract sender bsEncoded
            newContractAddress <- getContractAddress tx

            (Right code) <- runWeb3 $ getCode newContractAddress Latest
            actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
            pure ()
        ]

    -- , TestLabel "Append OpCodes" $ TestList $
    --     [ TestLabel "Should Produce Valid Code" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code of Increased Length" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Append the OpCodes" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code With Valid Jumps" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Maintain the Input Jump Locations" $ TestCase $ do
    --         undefined
    --     ]
    -- , TestLabel "Insert OpCodes" $ TestList $
    --     [ TestLabel "Should Produce Valid Code" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code of Increased Length" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Insert the OpCodes (Correct Location)" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code With Valid Jumps" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Maintain the Input Jump Locations" $ TestCase $ do
    --         undefined
    --     ]
    -- , TestLabel "Remove OpCodes" $ TestList $
    --     [ TestLabel "Should Produce Valid Code" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code of Decreased Length" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Remove the OpCodes (Correct Location)" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code With Valid Jumps" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Maintain the Input Jump Locations" $ TestCase $ do
    --         undefined
    --     ]
    -- , TestLabel "Insert and Remove Opcodes" $ TestList $
    --     [ TestLabel "Should Produce Valid Code" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Effect the Correct Changes" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Produce Code With Valid Jumps" $ TestCase $ do
    --         undefined
    --     , TestLabel "Should Maintain the Input Jump Locations" $ TestCase $ do
    --         undefined
    --     ]
    ]


adderProtectedOnChain = TestLabel "\"Adder\" on chain (transformed)" $ TestCase $ do
    -- Define test values.
    let testValueA = "0000000000000000000000000000000000000000000000000000000000000045"
        testValueB = "0000000000000000000000000000000000000000000000000000000000000003"
        testValueRes = "0000000000000000000000000000000000000000000000000000000000000048"

    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    newContractAddress <- deployFromFile (transform defaultCaps) "test/Models/Adder.sol"

    resRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "add" False
                    [ FunctionArg "a" "uint256"
                    , FunctionArg "b" "uint256"
                    ] (Just [FunctionArg "" "uint256"]))) <> testValueA <> testValueB)
            }
        theCall <- Eth.call details Latest
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    case resRaw of
        Left e -> assertFailure $ show e
        Right res -> assertEqual "Result" ("0x" <> testValueRes) res


adderOnChainUntransformed = TestLabel "\"Adder\" on chain (untransformed)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    newContractAddress <- deployFromFile id "test/Models/Adder.sol"
    -- testValueA + testValueB = testValueRes
    let testValueA = "0000000000000000000000000000000000000000000000000000000000000045"
        testValueB = "0000000000000000000000000000000000000000000000000000000000000003"
        testValueRes = "0000000000000000000000000000000000000000000000000000000000000048"
    resRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "add" False
                    [ FunctionArg "a" "uint256"
                    , FunctionArg "b" "uint256"
                    ] (Just [FunctionArg "" "uint256"]))) <> testValueA <> testValueB)
            }
        theCall <- Eth.call details Latest
        pure (theCall)
    case resRaw of
        Left e -> assertFailure $ show e
        Right res -> assertEqual "Result" ("0x" <> testValueRes) res


storeAndGetOnChainUnprotected  = TestLabel "\"StorerAndGetter\" on chain (unprotected, in bound)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    newContractAddress <- deployFromFile id "test/Models/StorerAndGetter.sol"

    -- We are able to run test jumps as this code is now in it's deployed form
    (Right code) <- runWeb3 $ getCode newContractAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
    testJumps actualRunCode

    let testValue = "0000000000000000000000000000000000000000000000000000000000000045"
    -- Use a call (send a transaction) to "store" to set a particular value
    (Right (storeRes)) <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "store" False
                    [ FunctionArg "loo" "uint256"
                    ] Nothing)) <> testValue)
            }
        theCall <- Eth.sendTransaction details
        pure (theCall)
    assertBool "Store result should not be zero" ("0x0" /= storeRes)
    --  Use a call to "get" to ensure that the stored value has been correctly set.
    (Right (getRes)) <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "get" False
                    [] (Just [FunctionArg "d" "uint256"]))))
            }
        theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    assertEqual "Result should be successfully retrieved" ("0x" <> testValue) getRes

storeAndGetOnChainProtected = TestLabel "\"StorerAndGetter\" on chain (protected, in bound)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    newContractAddress <- deployFromFile (transform defaultCaps) "test/Models/StorerAndGetter.sol"

    let testValue = "0000000000000000000000000000000000000000000000000000000000000045"
    -- Use a call (send a transaction) to "store" to set a particular value
    storeResRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "store" False
                    [ FunctionArg "loo" "uint256"
                    ] Nothing)) <> testValue)
            }
        theCall <- Eth.sendTransaction details
        pure (theCall)

    case storeResRaw of
        Right storeRes -> assertBool "Store Result" ("0x0" /= storeRes)
        Left e -> error $ "ssssseee" ++ show e
    --  Use a call to "get" to ensure that the stored value has been correctly set.
    getResRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "get" True
                    [] (Just [FunctionArg "d" "uint256"]))))
            }
        theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    getRes <- case getResRaw of
        Left e -> assertFailure $  ":LK " ++ show e
        Right x -> pure x
    assertEqual "Result" ("0x" <> testValue) getRes
    getLogsRaw <- runWeb3 $ do
        let details = (Filter
                { filterAddress = Just newContractAddress
                , filterTopics    = Just []
                , filterFromBlock = Earliest
                , filterToBlock = Latest
                })
        theLogs <- Eth.getLogs details
        pure (theLogs)
    case getLogsRaw of
        Left e -> assertFailure $  "logs " ++ show e
        Right logs -> do
            -- There will only be one log here because this is a new contract.
            assertEqual "There are the correct number of logs" 1 (length logs)
            let [log] = logs
            assertEqual "Log address is correct" newContractAddress (changeAddress log)
            case (Address.fromText $ T.take 40 $ T.drop 2 $ changeData log) of
                Left e -> assertFailure (show e)
                Right address -> assertEqual "Log data address is correct" newContractAddress address
            assertEqual "Log data storage key is correct" "0100000100000000000000000000000000000000000000000000000000000000" (T.drop 40 $ T.drop 2 $ changeData log)

storeAndGetOnChainProtectedOutOfBounds = TestLabel "\"StorerAndGetter\" on chain (protected, out of bounds)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    let caps = Capabilities
            { caps_storageRange  = (0x0200000000000000000000000000000000000000000000000000000000000000,0x0300000000000000000000000000000000000000000000000000000000000000)
            }
    newContractAddress <- deployFromFile (transform caps) "test/Models/StorerAndGetter.sol"
    -- Because the store transaction should not have succeeded, this should still be zero.
    let testValue = "0000000000000000000000000000000000000000000000000000000000000000"
    -- Use a call (send a transaction) to "store" to set a particular value
    storeResRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "store" False
                    [ FunctionArg "loo" "uint256"
                    ] Nothing)) <> testValue)
            }
        theCall <- Eth.call details Latest
        theEffect <- Eth.sendTransaction details
        pure (theCall, theEffect)
    -- The transaction should fail
    case storeResRaw of
        Right (theCall, theEffect) -> assertFailure "Should not succeed"
        Left e -> pure ()
    -- Use a call to "get" to ensure that the stored value has been correctly set.
    getResRaw <- runWeb3 $ do
        sender <- fmap (!! 1) accounts
        let details = Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "get" True
                    [] (Just [FunctionArg "d" "uint256"]))))
            }
        theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    getRes <- case getResRaw of
        Left e -> assertFailure $  ":LK " ++ show e
        Right x -> pure x
    assertEqual "Result" ("0x" <> testValue) getRes
    -- check the presence of the logs

deployFromFile :: ([OpCode] -> [OpCode]) -> FilePath -> IO Address
deployFromFile transform filepath = do
    -- TODO: handle exceptions
    bsEncodedFull <- compileSolidityFileBinFull filepath
    bsEncodedRunTime <- compileSolidityFileBinRunTime filepath
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
    let bsEncoded = B16.encode $ B.concat $ map toByteString $ transform bytecode
    newContractAddressRaw <- runWeb3 $ deployContractDefault bsEncoded
    case newContractAddressRaw of
        Right x -> pure x
        Left e -> error $ "Contract deployment failure: " ++ show e

deployContractDefault bsEncoded = do
    availableAccounts <- accounts
    let sender = availableAccounts !! 1
    (res, tx) <- deployContract' sender bsEncoded
    newContractAddressRaw <- getContractAddress' tx
    let newContractAddress = case newContractAddressRaw of
            Nothing -> error "contract no successfully deployed"
            Just x -> x
    code <- getCode newContractAddress Latest
    pure newContractAddress
    -- assertEqual "Deployed bytecode is as expected" ("0x" <> (T.pack $ C8.unpack bsEncoded)) code

deployContract sender bsEncoded =  do
    r <- runWeb3 $ do
        let details = Call {
                callFrom = Just sender,
                callTo = Nothing,
                callGas = Just 900000,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just (T.pack $ C8.unpack $ "0x" `B.append` bsEncoded)
            }
        theCall <- Eth.call details Latest
        theEffect <- Eth.sendTransaction details
        pure (theCall, theEffect)
    case r of
        Left e -> assertFailure $ show e
        Right x -> pure x

deployContract' sender bsEncoded =do
    let details = Call {
            callFrom = Just sender,
            callTo = Nothing,
            callGas = Just 900000,
            callGasPrice = Nothing,
            callValue = Nothing,
            callData = Just (T.pack $ C8.unpack $ "0x" `B.append` bsEncoded)
        }
    theCall <- Eth.call details Latest
    theEffect <- Eth.sendTransaction details
    pure (theCall, theEffect)

getContractAddress tx = do
    contractAddressResult <- runWeb3 $ do
        r <- getTransactionReceipt tx
        pure $ txrContractAddress r
    case contractAddressResult of
            Left e -> assertFailure ("ss" ++ show e)
            Right (Just x) -> pure x
            Right Nothing -> assertFailure "No new contract address was returned"

getContractAddress' tx = do
        r <- getTransactionReceipt tx
        pure $ txrContractAddress r
