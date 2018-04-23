{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Exception

import Data.Attoparsec.ByteString
import qualified Text.Parsec.Prim as Parsec
import qualified Text.Parsec.Combinator as Parsec
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
import OpCode.StructureParser
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
    [ testGroup "OpCode Parser" $ (hUnitTestToTests Tests.HandleOpCodes.parserTests)
    , testGroup "Preprocessor" $ (hUnitTestToTests Tests.Transform.preprocessorTests)
    , testProperty "Round-Trip Single OpCode" Main.prop_anyValidOpCode_roundTrip
    , testProperty "Round-Trip Full Bytecode" Main.prop_anyValidBytecode_roundTrip
    , testProperty "Monotonic Counted Bytecode" Tests.HandleOpCodes.prop_anyCountedBytecode_monotonic
    , testProperty "Preserved Counted Bytecode" Tests.HandleOpCodes.prop_anyCountedBytecode_codePreserved
    , testGroup "OpCode Checks" $ (hUnitTestToTests Tests.Analyse.storeCheckerTests)
    , testGroup "Number Handling" $ (hUnitTestToTests Main.numberTests)
    , testProperty "Round-Trip Natural to Bytecode" Main.prop_integerToEVM256_roundTrip
    , testGroup "Web3 Library" $ hUnitTestToTests Main.web3Tests
    , testGroup "Structure Parser" $ hUnitTestToTests Main.structureParserTests
    ]

structureParserTests = TestList
    [ TestLabel "Parse single opcode" $ TestCase $ do
        let parseRes = Parsec.parse (opCode POP) "TestInput" [POP]
        assertEqual "Parsed value should be POP" (Right POP) parseRes
    , TestLabel "Parse push value" $ TestCase $ do
        let parseRes = Parsec.parse (pushVal) "TestInput" [PUSH1 (B.pack [0x4])]
        assertEqual "Parsed value should be 0x4" (Right 0x4) parseRes
    , TestLabel "Fail to parse single opcode" $ TestCase $ do
        let parseRes = Parsec.parse (opCode XOR) "TestInput" [POP]
        case parseRes of
            Left _ -> pure ()
            Right r -> assertFailure (show r ++ " should not have been parsed")
    , TestLabel "Simple combo (no push)" $ TestCase $ do
        let testCombo =
                [ POP
                , XOR
                , ADDRESS
                ]
            notTestCombo =
                [ POP
                , XOR
                , REVERT
                ]
            testComboParser = do
                opCode POP
                opCode XOR
                opCode ADDRESS
                pure ()
        let parseResGood = Parsec.parse testComboParser "TestInput" testCombo
        assertEqual
            "Input should be parsed and return nothing (as in \"()\")"
            (Right ())
            parseResGood
        let parseResBad = Parsec.parse testComboParser "TestInput" notTestCombo
        case parseResBad of
            Left _ -> pure ()
            Right r -> assertFailure (show r ++ " should not have been parsed")
    , TestLabel "Protected call" $ TestCase $ do
        let ll = 0x1
            ul = 0x2
            topic = keccak256Bytes "KERNEL_SSTORE"
        let testCombo = protectedStoreExample ll ul
            notTestCombo =
                [ POP
                , XOR
                , REVERT
                ]
        let parseResGood = Parsec.parse parseLoggedAndProtectedSSTORE "TestInput" testCombo
        assertEqual
            "Input should be parsed and return the correct store range"
            (Right (1,2))
            parseResGood
        let parseResBad = Parsec.parse parseLoggedAndProtectedSSTORE "TestInput" notTestCombo
        case parseResBad of
            Left _ -> pure ()
            Right r -> assertFailure (show r ++ " should not have been parsed")
    , TestLabel "Protected call (full parse)" $ TestCase $ do
        let ll = 0x1
            ul = 0x2
        let testCombo = protectedStoreExample ll ul
            notTestCombo =
                [ POP
                , XOR
                , REVERT
                ]
        let parseRes = fullStructuredParse testCombo
        assertEqual
            "Input should be parsed and return a single protected store with the correct store range"
            (Right [ProtectedStoreCall (ll,ul)])
            parseRes
    , TestLabel "Protected call (full parse) plus padding" $ TestCase $ do
        let ll = 0x1
            ul = 0x2
        let testCombo = notTestCombo ++ (protectedStoreExample ll ul) ++ notTestCombo
            notTestCombo =
                [ POP
                , XOR
                , REVERT
                , SSTORE
                ]
        let parseRes = fullStructuredParse testCombo
        assertEqual
            "Input should be parsed and return a single protected store with the correct store range"
            (Right
                [ OtherOpCode POP
                , OtherOpCode XOR
                , OtherOpCode REVERT
                , UnprotectedStoreCall
                , ProtectedStoreCall (ll,ul)
                , OtherOpCode POP
                , OtherOpCode XOR
                , OtherOpCode REVERT
                , UnprotectedStoreCall
                ])
            parseRes
    , TestLabel "Check for sequence" $ TestCase $ do
        -- In this example we want to find if the byte code as any examples
        -- where PUSH is followed directly by POP (a useless operation). We also
        -- want to return the list of values that we pushed in this way.
        -- First a push val (we keep)
        -- Then a pop
        -- Return the push val we found (using pure)
        let sequenceParser = do
               v <- pushVal
               opCode POP
               pure v
            -- This will find all the sequences in a row
            findAllConsecutiveSequences = Parsec.many (Parsec.try sequenceParser)
            -- But this version discards everything else and finds ALL sequences
            findAllSequences = Parsec.many $ Parsec.try $ do
                    skip
                    sequenceParser -- parse sequence
                where
                    skip = Parsec.manyTill anyOpCode (Parsec.try $ Parsec.lookAhead sequenceParser)
        let testCode =
                [ PUSH1 (B.pack [0x55])
                , POP
                , PUSH1 (B.pack [0x88])
                , POP
                , PUSH1 (B.pack [0x99])
                , PUSH1 (B.pack [0xbb])
                , POP
                ]
        case Parsec.parse findAllSequences "testCode" testCode of
                Left e -> error $ show e -- We weren't able to satusfy our pattern
                Right xs -> assertEqual "Should find all values except 0x99, as it is not followd by a POP" [0x55,0x88,0xbb] xs
        , TestLabel "Validate Specific Code" $ TestCase $ do
            -- In this example we want our bytecode to match *exactly*. However,
            -- during compilation a contract address for CALL is determined. We
            -- don't know what it should be, so we should leave a "hole" in out
            -- pattern. (NB: This contract doesn't actually work).
            let exactBytecodeParser = do
                    opCode (PUSH1 $ B.pack [0x32])
                    opCode STOP
                    opCode STOP
                    address <- pushVal -- Some address, (our "hole")
                    opCode CALL
                    opCode (PUSH1 $ B.pack [0x40])
                    opCode (PUSH1 $ B.pack [0x60])
                    opCode RETURN
                    Parsec.eof -- End of file/input. Means this is the entire bytecode and nothing follows.
                    pure address -- return the address
            let exampleAddress = 123456789123456789123 :: Natural
            let testCodeBad =
                    [ PUSH1 (B.pack [0x32])
                    , STOP
                    , STOP
                    , PUSH32 $ integerToEVM256 exampleAddress
                    , CALL
                    , PUSH1 (B.pack [0x40])
                    , PUSH1 (B.pack [0x60])
                    , RETURN
                    , REVERT -- This will not pass because it has a trailing REVERT (and we specified eof)
                    ]
                testCodeGood =
                    [ PUSH1 (B.pack [0x32])
                    , STOP
                    , STOP
                    , PUSH32 $ integerToEVM256 exampleAddress
                    , CALL
                    , PUSH1 (B.pack [0x40])
                    , PUSH1 (B.pack [0x60])
                    , RETURN
                    ]
            case Parsec.parse exactBytecodeParser "testCodeBad" testCodeBad of
                -- The bytecode failed to match our pattern because of the trailing REVERT (hence Left)
                Left e -> pure ()
                Right xs -> assertFailure "This code should have failed to be matched by the parser"
            case Parsec.parse exactBytecodeParser "testCodeGood" testCodeGood of
                    Left e -> error $ show e -- We weren't able to satisfy our pattern
                    Right xs -> assertEqual "Should find the address we used" exampleAddress xs
    ]

protectedStoreExample ll ul =
    [ PUSH32 $ integerToEVM256 ll -- lower limit
    , DUP2 -- duplicate store address for comparison
    , OpCode.Type.LT -- see if address is lower than the lower limit
    , PUSH32 $ integerToEVM256 ul -- upper limit
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
    where
        topic = keccak256Bytes "KERNEL_SSTORE"

-- |This is a test of tests to ensure the methodology for web3 testing is
-- correct.
web3Tests :: Test
web3Tests = TestLabel "Web3" $ TestCase $ do
    bsEncodedFull <- compileSolidityFileBinFull "test/Models/Adder.sol"
    let bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
        bsEncoded = B16.encode bsDecodedFull
    (Right availableAccounts) <- runWeb3 accounts
    let sender =  case availableAccounts of
            [] -> error "No accounts available"
            (a:_) -> a
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
