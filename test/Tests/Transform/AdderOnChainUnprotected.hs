{-# LANGUAGE OverloadedStrings #-}
module Tests.Transform.AdderOnChainUnprotected where

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


adderOnChainUntransformed = TestLabel "\"Adder\" on chain (untransformed)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    newContractAddress <- deployFromFile id "test/Models/Adder.sol"
    assertBool "The address should not be the zero address" (newContractAddress /= Address.zero)
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
