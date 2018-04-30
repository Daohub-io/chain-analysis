{-# LANGUAGE OverloadedStrings #-}
module Tests.Utils where

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

nullRes :: Text
nullRes = "0x0"

runWeb3 :: Web3 a -> IO (Either Web3Error a)
runWeb3 = runWeb3' (HttpProvider "http://localhost:8545")

parseGoodExample bytecode =
  case parse (parseOpCodes <* endOfInput) bytecode `feed` "" of
      Fail i contexts err -> assertFailure $ "Opcodes should be parsed in full: " ++ show contexts ++ " " ++ err ++ " remaining: " ++ show (encode i)
      Partial f -> error $ show (f "")
      Done i r -> pure r

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
    accs <- accounts
    let sender = case accs of
            [] -> error "No accounts available"
            (a:_) -> a
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
                callGas = Just 3000000,
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
            callGas = Just 3000000,
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

getAllLogs :: Address -> Web3 [Change]
getAllLogs contractAddress = do
    let details = (Filter
            { filterAddress = Just contractAddress
            , filterTopics    = Just []
            , filterFromBlock = Earliest
            , filterToBlock = Latest
            })
    theLogs <- Eth.getLogs details
    pure (theLogs)

parseStorageLog :: Change -> (Address, Text)
parseStorageLog log = (address, storageKey)
    where
        storageKey = T.drop 40 $ T.drop 2 $ changeData log
        (Right address) = Address.fromText $ T.take 40 $ T.drop 2 $ changeData log

-- |Storage logs should also be ordered.
checkStorageLogs :: [(Address, Text)] -> [Change] -> Assertion
checkStorageLogs expectedLogInfo logs = do
    assertEqual "The number of logs should be equal to the number expected"
        (length expectedLogInfo) (length logs)
    mapM_ (\((a,b),c) -> checkStorageLog a b c) (zip expectedLogInfo logs)

checkStorageLog :: Address -> Text -> Change -> Assertion
checkStorageLog expectedContractAddress expectedStorageKey log = do
    let (logContractAddress, logStorageKey) = parseStorageLog log
    assertEqual "Log address should be correct" expectedContractAddress (changeAddress log)
    assertEqual "Log data address should be correct" expectedContractAddress logContractAddress
    assertEqual "Log data storage key should be correct" expectedStorageKey logStorageKey
