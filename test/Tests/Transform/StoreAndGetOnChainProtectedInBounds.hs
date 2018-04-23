{-# LANGUAGE OverloadedStrings #-}
module Tests.Transform.StoreAndGetOnChainProtectedInBounds (test) where

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

import Test.QuickCheck
import Test.HUnit hiding (test)

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

test :: Test
test = TestLabel "\"StorerAndGetter\" on chain (protected, in bound)" $ TestList
    [ TestLabel "Contract Deployment" $ TestCase $ do
        -- #Deployment
        -- Take the contract from the file and deploy it, returning the address.
        newContractAddress <- deployFromFile transformation "test/Models/StorerAndGetter.sol"
        assertBool "The address should not be the zero address" (newContractAddress /= Address.zero)
    , TestLabel "Store Value" $ TestCase $ do
        -- #Deployment
        -- Take the contract from the file and deploy it, returning the address.
        newContractAddress <- deployFromFile transformation "test/Models/StorerAndGetter.sol"
        -- #Store
        -- Store the test value (using the storeValue function defined below)
        storeResult <- storeValue newContractAddress
        -- If the store is successful, check that the result is a valid transaction
        -- hash, otherwise failure.
        case storeResult of
            Right (storeRes, storeTx) -> do
                assertEqual "Store result should be null (there is no return value)" nullRes storeRes
                assertBool "Store transaction hash should not be zero" (nullRes /= storeTx)
            Left e -> assertFailure $ "Store call should succeed (" ++ show e ++ ")"
    , TestLabel "Retrieve Value" $ TestCase $ do
        -- #Deployment
        -- Take the contract from the file and deploy it, returning the address.
        newContractAddress <- deployFromFile transformation "test/Models/StorerAndGetter.sol"
        -- #Store
        -- Store the test value (using the storeValue function defined below)
        storeResult <- storeValue newContractAddress
        -- If the store is successful, check that the result is a valid transaction
        -- hash, otherwise failure.
        case storeResult of
            Right (storeRes, storeTx) -> do
                assertEqual "Store result should be null (there is no return value)" nullRes storeRes
                assertBool "Store transaction hash should not be zero" (nullRes /= storeTx)
            Left e -> assertFailure $ "Store call should succeed (" ++ show e ++ ")"
        -- #Retrieve
        -- Retrieve the value (using the retrieveValue function defined below)
        retrieveResult <- retrieveValue newContractAddress
        -- If the retrieval is successful, check that the result is the expected
        -- value.
        case retrieveResult of
            Right getRes -> assertEqual "The retrieved result should be the same as the one stored" ("0x" <> testValue) getRes
            Left e -> assertFailure $  "A value should be successfully retrieved (" ++ show e ++ ")"
    , TestLabel "Retrieve Logs" $ TestCase $ do
        -- #Deployment
        -- Take the contract from the file and deploy it, returning the address.
        newContractAddress <- deployFromFile transformation "test/Models/StorerAndGetter.sol"
        -- #Store
        -- Store the test value (using the storeValue function defined below)
        storeResult <- storeValue newContractAddress
        -- If the store is successful, check that the result is a valid transaction
        -- hash, otherwise failure.
        case storeResult of
            Right (storeRes, storeTx) -> do
                assertEqual "Store result should be null (there is no return value)" nullRes storeRes
                assertBool "Store transaction hash should not be zero" (nullRes /= storeTx)
            Left e -> assertFailure $ "Store call should succeed (" ++ show e ++ ")"
        -- #Retrieve
        -- Retrieve the value (using the retrieveValue function defined below)
        retrieveResult <- retrieveValue newContractAddress
        -- If the retrieval is successful, check that the result is the expected
        -- value.
        case retrieveResult of
            Right getRes -> assertEqual "The retrieved result should be the same as the one stored" ("0x" <> testValue) getRes
            Left e -> assertFailure $  "A value should be successfully retrieved (" ++ show e ++ ")"
        -- #View Logs
        -- Retrieve the logs from this contract.
        (Right logs) <- runWeb3 $ getAllLogs newContractAddress
        -- Ensure that they are as expected
        let expectedLogs =
                [ (newContractAddress, "0100000100000000000000000000000000000000000000000000000000000000")
                ]
        checkStorageLogs expectedLogs logs
    ]
    where
        transformation = transform caps
        caps = defaultCaps
        testValue = "0000000000000000000000000000000000000000000000000000000000000045"
        -- Use a call (send a transaction) to "store" to set a particular value
        storeValue newContractAddress = runWeb3 $ do
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
                    callData = Just ((JsonAbi.methodId (DFunction "store" False
                        [ FunctionArg "loo" "uint256"
                        ] Nothing)) <> testValue)
                }
            -- Using call will allow us to immediately see informtion about the
            -- success or failure of the transaction, but has no effect.
            theCall <- Eth.call details Latest
            -- Using sendTransaction will effect the state change, but is opaque
            -- and will not inform us of success or failure.
            theEffect <- Eth.sendTransaction details
            pure (theCall, theEffect)
        --  Use a call to "get" to ensure that the stored value has been correctly set.
        retrieveValue newContractAddress = runWeb3 $ do
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
                    callData = Just ((JsonAbi.methodId (DFunction "get" True
                        [] (Just [FunctionArg "d" "uint256"]))))
                }
            theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
            -- theCall <- Eth.sendTransaction details
            pure (theCall)
