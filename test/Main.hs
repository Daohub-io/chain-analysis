{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

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

-- Import code for parsing opcodes and all low level data handling.
import Tests.HandleOpCodes
-- Import code that can analyse the opcodes and perform tests and checks
import Tests.Analyse
-- Import code that can perform transformations on the opcodes
import Tests.Transform
-- Import the base utilities that are shared with all test code
import Tests.Utils

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

tests = -- [ testGroup "Single Test" $ hUnitTestToTests storeAndGetOnChainProtected ]
    [ testGroup "OpCode Parser" $ (hUnitTestToTests parserTests)
    , testGroup "Preprocessor" $ (hUnitTestToTests preprocessorTests)
    , testProperty "Round-Trip Single OpCode" prop_anyValidOpCode_roundTrip
    , testProperty "Round-Trip Full Bytecode" prop_anyValidBytecode_roundTrip
    , testProperty "Monotonic Counted Bytecode" prop_anyCountedBytecode_monotonic
    , testProperty "Preserved Counted Bytecode" prop_anyCountedBytecode_codePreserved
    , testGroup "OpCode Checks" $ (hUnitTestToTests storeCheckerTests)
    , testGroup "Number Handling" $ (hUnitTestToTests numberTests)
    , testProperty "Round-Trip Natural to Bytecode" prop_integerToEVM256_roundTrip
    , testGroup "Web3 Library" $ hUnitTestToTests web3Tests
    ]

-- |This is a test of tests to ensure the methodology for web3 testing is
-- correct.
web3Tests :: Test
web3Tests = TestLabel "Web3" $ TestCase $ do
    bsEncodedFull <- compileSolidityFileBinFull "test/Models/Adder.sol"
    let bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
        bsEncoded = B16.encode bsDecodedFull
    (Right availableAccounts) <- runWeb3 accounts
    let sender = availableAccounts !! 1
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx
    (Right (res)) <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "add" False
                    [ FunctionArg "a" "uint256"
                    , FunctionArg "b" "uint256"
                    ] (Just [FunctionArg "" "uint256"]))) <> "0000000000000000000000000000000000000000000000000000000000000045" <> "0000000000000000000000000000000000000000000000000000000000000001")
            })

        theCall <- Eth.call details Latest
        pure (theCall)
    assertEqual "Result" "0x0000000000000000000000000000000000000000000000000000000000000046" res

numberTests :: Test
numberTests = TestLabel "Numbers" $ TestList
    [ naturalRoundTrip 0x00
    , naturalRoundTrip 0x0011
    , naturalRoundTrip 0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    ]

naturalRoundTrip :: Integer -> Test
naturalRoundTrip number = TestLabel ("Parse " ++ show number) $ TestCase $
    assertBool
        (show (abs number) ++ " /= " ++ show (encode (integerToEVM256 (fromIntegral (abs number)))))
        (prop_integerToEVM256_roundTrip number)


prop_integerToEVM256_roundTrip :: Integer -> Bool
prop_integerToEVM256_roundTrip int =
    let nat = abs int
        bs = integerToEVM256 (fromIntegral nat)
        num = evm256ToInteger bs
    in nat == (fromIntegral num)

prop_anyValidOpCode_roundTrip :: OpCode -> Bool
prop_anyValidOpCode_roundTrip opCode =
    let bs = toByteString opCode
        parseResult = parse parseOpCode bs
    in case parseResult of
        Fail i contexts err -> False
        -- Partial _ ->
        Done i oc -> True

prop_anyValidBytecode_roundTrip :: [OpCode] -> Bool
prop_anyValidBytecode_roundTrip bytecode =
    let bs = B.concat $ map toByteString bytecode
        parseResult = parse parseOpCodes bs
    in case parse (parseOpCodes <* endOfInput) bs `feed` "" of
            Fail i contexts err -> False
            Partial f -> error $ "impossible error"
            Done i r -> True
