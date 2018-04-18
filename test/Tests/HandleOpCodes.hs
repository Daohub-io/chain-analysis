{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.HandleOpCodes where

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

prop_anyCountedBytecode_monotonic :: [OpCode] -> Bool
prop_anyCountedBytecode_monotonic bytecode =
    let countedBytecode = countCodes bytecode
    in isMonotonic countedBytecode

prop_anyCountedBytecode_codePreserved :: [OpCode] -> Bool
prop_anyCountedBytecode_codePreserved bytecode =
    let countedBytecode = countCodes bytecode
    in (length countedBytecode == length bytecode)
        && (and $ zipWith sameOpCode bytecode countedBytecode)
    where
        sameOpCode :: OpCode -> CountedOpCode -> Bool
        sameOpCode opCodeO (opCodeN,_) = opCodeO == opCodeN

-- Parse each of the opcodes individually.
-- TODO: finish this list.
singleOpCodes = TestLabel "SingleOpCodes" $ TestList
    [ singleParseTest parseSTOP STOP (pack [0x00])
    , singleParseTest parseADD ADD (pack [0x01])
    , singleParseTest parseMUL MUL (pack [0x02])
    , singleParseTest parseSUB SUB (pack [0x03])
    , singleParseTest parseDIV DIV (pack [0x04])
    , singleParseTest parseSDIV SDIV (pack [0x05])
    , singleParseTest parseMOD MOD (pack [0x06])
    , singleParseTest parseSMOD SMOD (pack [0x07])
    , singleParseTest parseADDMOD ADDMOD (pack [0x08])
    , singleParseTest parseMULMOD MULMOD (pack [0x09])
    , singleParseTest parseEXP EXP (pack [0x0a])
    , singleParseTest parseSIGNEXTEND SIGNEXTEND (pack [0x0b])

    ]

antiSingleOpCodes = TestLabel "AntiSingleOpCodes" $ TestList
    [ antiSingleParseTest (pack [0x1b])
    ]

singleParseTest parser target unparsed = TestLabel ("Parse " ++ show target ++ " OpCode") $ TestCase $ do
    case parseOnly (parser <* endOfInput) unparsed of
        Left err -> assertFailure $ "Opcodes should be parsed in full: " ++ err
        Right parsed -> assertEqual "parsed and target should be equal" parsed target
    -- For the opposite test, if input is MUL, use SUB, else use MUL
    let contraCase = if unparsed == pack [0x02] then pack [0x03] else pack [0x02]
    case parseOnly (parser <* endOfInput) contraCase of
        Left err -> pure ()
        Right parsed -> assertBool "parsed and target should not be equal" (parsed /= target)

antiSingleParseTest bytecode = TestLabel ("Parse unknown bytecode " ++ show (encode bytecode)) $ TestCase $ do
    case parseOnly (parseOpCode <* endOfInput) bytecode of
        Left err -> pure ()
        Right parsed -> assertFailure $ "Bytecode should not be successfully parsed: " ++ show parsed

parseGoodExampleTest bytecode = TestLabel "Parse Good Example" $ TestCase
    $ (parseGoodExample bytecode >> pure ())

-- parseGoodExample bytecode =
--     case parseOnly (parseOpCodes <* endOfInput) bytecode of
--         Left err -> assertFailure $ "Opcodes should be parsed in full: " ++ err
--         Right parsed -> pure ()

parseBadExampleTest bytecode = TestLabel "Parse Bad Example" $ TestCase $ do
    case parseOnly (parseOpCodes <* endOfInput) bytecode of
        Left err -> pure ()
        Right parsed -> assertFailure $ "Opcodes should not be parsed : " ++ show parsed

-- Parse something that is not the STOP OpCode
parseSTOPTestNot = TestLabel "Parse STOP OpCode Not" $ TestCase $ do
    case parseOnly (parseSTOP <* endOfInput) (pack [0x01]) of
        Left _ -> pure ()
        Right oc -> assertFailure $ "No opcode should be parsed, but " ++ show oc ++ " was parsed"

testExampleContract = TestLabel "Parse Example Contract" $ TestCase $ do
    let (bs,_) = decode $ C8.pack "6060604052341561000f57600080fd5b60ba8061001d6000396000f300606060405260043610603f576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063771602f7146044575b600080fd5b3415604e57600080fd5b606b60048080359060200190919080359060200190919050506081565b6040518082815260200191505060405180910390f35b60008183019050929150505600a165627a7a723058203691f76bc66e6d69f31bbfe2298197772c0c4c64b8eaefe9eec731a4e91fc1a50029"
    case parse (parseOpCodes <* endOfInput) bs of
        Fail i contexts err ->
            assertFailure $ "Opcodes should be parsed in full, remainder: " ++ show (encode i)
        Done i ocs -> do
            mapM_ print ocs
            pure ()

parserTests = TestLabel "OpCode Parser" $ TestList $
    [ TestLabel "Should Parse Empty Code" $ TestCase $ do
        let (bytecode,_) = decode $ C8.pack ""
        case parseOnly (parseOpCodes <* endOfInput) bytecode of
            Left err -> assertFailure $ "Opcodes should be parsed in full: " ++ err
            Right parsed -> assertEqual "parsed and target should be equal" parsed []
    , TestLabel "Should All Single Valid OpCodes" singleOpCodes
    , TestLabel "Should Reject Single Invalid OpCodes" antiSingleOpCodes
    , TestLabel "Should Parse Swarm Metadata" $ TestCase $ do
        let (bytecode,_) = decode "a165627a7a723058203691f76bc66e6d69f31bbfe2298197772c0c4c64b8eaefe9eec731a4e91fc1a50029"
        case parseOnly (parseSwarmMetadata <* endOfInput) bytecode of
            Left err -> assertFailure $ "Opcodes should be parsed in full: " ++ err
            Right parsed -> assertEqual "parsed and target should be equal" parsed ()
    , TestLabel "Should Parse Simple Hand-Written Samples" $ TestList
        [ TestLabel "Should Parse Good Examples" $ TestList $
            map parseGoodExampleTest hwsPass
        , TestLabel "Should Not Parse Bad Examples" $ TestList $
            map parseBadExampleTest hwsFail
        ]
    , TestLabel "Should Parse Compiled Examples" $ TestList $
        [ TestLabel "Should Parse \"Storer\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Storer.sol"
            parseGoodExample bsDecoded >> pure ()
        , TestLabel "Should Parse \"Adder\"" $ TestCase $ do
            -- Read in the test data file
            bsDecoded <- compileSolidityFileBin "test/Models/Adder.sol"
            parseGoodExample bsDecoded >> pure ()
        , TestLabel "Should Parse \"Adder\" Without Swarm Metadata" $ TestCase $ do
            -- Read in the test data file
            bsEncoded <- B.readFile "test/Models/AdderWithoutSwarmMetadata.dat"
            -- Decode the hex string from the file
            let (bsDecoded,"") = decode bsEncoded
            parseGoodExample bsDecoded >> pure ()
        ]
    ]