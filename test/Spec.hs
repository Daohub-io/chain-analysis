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

import Network.Ethereum.Web3
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
-- import Network.Ethereum.Web3.TH
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

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

tests = -- [ testGroup "Adder Protected On Chain" $ hUnitTestToTests adderProtectedOnChain ]
    [ testGroup "OpCode Parser" $ (hUnitTestToTests parserTests)
    , testGroup "Preprocessor" $ (hUnitTestToTests preprocessorTests)
    , testProperty "Round-Trip Single OpCode" prop_anyValidOpCode_roundTrip
    , testProperty "Round-Trip Full Bytecode" prop_anyValidBytecode_roundTrip
    , testProperty "Monotonic Counted Bytecode" prop_anyCountedBytecode_monotonic
    , testProperty "Preserved Counted Bytecode" prop_anyCountedBytecode_codePreserved
    , testGroup "Checks" $ (hUnitTestToTests storeCheckerTests)
    , testGroup "Number" $ (hUnitTestToTests numberTests)
    , testProperty "Round-Trip Natural to Bytecode" prop_integerToEVM256_roundTrip
    , testGroup "Web3" $ hUnitTestToTests web3Tests
    ]

-- |This is a test of tests to ensure the methodology for web3 testing is
-- correct.
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

numberTests = TestLabel "Numbers" $ TestList
    [ naturalRoundTrip 0x00
    , naturalRoundTrip 0x0011
    , naturalRoundTrip 0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    ]

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

parseGoodExample bytecode =
    case parse (parseOpCodes <* endOfInput) bytecode `feed` "" of
        Fail i contexts err -> assertFailure $ "Opcodes should be parsed in full: " ++ show contexts ++ " " ++ err ++ " remaining: " ++ show (encode i)
        Partial f -> error $ show (f "")
        Done i r -> pure r

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
                    , SSTORE -- perform the store
                    ]
            assertBool "Protected stored calls should pass store checker" (checkStores code)
            assertEqual "Protected store a calls should match the required capabilities"
                (Ranges $ S.fromList [(lowerLimit, upperLimit)])
                (getRequiredCapabilities code)
        , TestLabel "\"StoreProtetectedInline\"" $ TestCase $ do
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
            assertEqual "Code should remain unchanged" code (transform code)
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
            assertEqual "Table should be added" expected (transform code)
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
        --         transformed = transform code
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
        --         transformed = transform code
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
            assertBool "After transformation should pass store checker" (checkStores $ transform code)
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
            assertBool "After transformation should pass store checker" (checkStores $ transform code)
        , TestLabel "\"StorerWithAdd\" on chain" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsEncodedFull <- compileSolidityFileBinFull "test/Models/StorerWithAdd.sol"
            bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/StorerWithAdd.sol"
            let
                bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
                bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
                    in if remainder == B.empty then bytes else error (show remainder)
            let bsEncoded = B16.encode bsDecodedFull
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
                        callData = Just ((JsonAbi.methodId (DFunction "storeWithAdd" False
                            [ FunctionArg "a" "uint256"
                            , FunctionArg "b" "uint256"
                            ] (Just [FunctionArg "" "uint256"]))) <> "0000000000000000000000000000000000000000000000000000000000000045" <> "0000000000000000000000000000000000000000000000000000000000000001")
                    })

                theCall <- Eth.call details Latest
                pure (theCall)
            assertEqual "Result" "0x0000000000000000000000000000000000000000000000000000000000000046" res
        , storeAndGetOnChainUnprotected
        , storeAndGetOnChainProtected
        , TestLabel "\"Adder\" on chain (untransformed)" $ TestCase $ do
            -- Read in the test Solidity source file. This file contains a
            -- Solidity contract with a single unprotected SSTORE call.
            bsEncodedFull <- compileSolidityFileBinFull "test/Models/Adder.sol"
            bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/Adder.sol"
            let
                bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
                    in if remainder == B.empty then bytes else error (show remainder)
                bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
                    in if remainder == B.empty then bytes else error (show remainder)
            bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
            let bsEncoded = B16.encode $ B.concat $ map toByteString $ bytecode
            (Right availableAccounts) <- runWeb3 accounts
            let sender = availableAccounts !! 1
            (res, tx) <- deployContract sender bsEncoded
            newContractAddress <- getContractAddress tx
            (Right code) <- runWeb3 $ getCode newContractAddress Latest
            -- assertEqual "Deployed bytecode is as expected" ("0x" <> (T.pack $ C8.unpack bsEncoded)) code
            let testValueA = "0000000000000000000000000000000000000000000000000000000000000045"
                testValueB = "0000000000000000000000000000000000000000000000000000000000000003"
                testValueRes = "0000000000000000000000000000000000000000000000000000000000000048"
            resRaw <- runWeb3 $ do
                let details = (Call {
                        callFrom = Just sender,
                        callTo = Just newContractAddress,
                        callGas = Nothing,
                        callGasPrice = Nothing,
                        callValue = Nothing,
                        callData = Just ((JsonAbi.methodId (DFunction "add" False
                            [ FunctionArg "a" "uint256"
                            , FunctionArg "b" "uint256"
                            ] (Just [FunctionArg "" "uint256"]))) <> testValueA <> testValueB)
                    })
                theCall <- Eth.call details Latest
                pure (theCall)
            case resRaw of
                Left e -> assertFailure $ show e
                Right res -> assertEqual "Result" ("0x" <> testValueRes) res
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

            B.writeFile "TrivialBuildA.txt" $ B16.encode $ B.concat $ map toByteString bytecode
            writeFile "TrivialBuild.txt"  $ unlines $ map show $ countCodes bytecode

            (Right code) <- runWeb3 $ getCode newContractAddress Latest
            actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
            T.writeFile "TrivialRunA.txt" $ code
            writeFile "TrivialRun.txt" $ unlines $ map show $ countCodes actualRunCode
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
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    -- bsDecoded <- compileSolidityFileBin "test/Models/Adder.sol"
    bsEncodedFull <- compileSolidityFileBinFull "test/Models/Adder.sol"
    bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/Adder.sol"
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
    let bsEncoded = B16.encode $ B.concat $ map toByteString $ transform bytecode
    (Right availableAccounts) <- runWeb3 accounts
    let sender = availableAccounts !! 1
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx
    (Right code) <- runWeb3 $ getCode newContractAddress Latest

    r <- Control.Exception.try $ parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code :: IO (Either SomeException [OpCode])
    let actualRunCode = case r of
            Right x -> x
            Left e -> error ("rqqrrt" ++ show e)
    writeFile "tbuildAdderTransformedPre.txt" $ unlines $ map show $ countCodes $ bytecode
    writeFile "tbuildAdderTransformedMid.txt" $ unlines $ map show $ (\(_,_,c)->c) $ midTransform bytecode
    writeFile "tbuildAdderTransformed.txt" $ unlines $ map show $ countCodes $ transform bytecode
    writeFile "trunAdderTransformed.txt" $ unlines $ map show $ countCodes $ actualRunCode
    -- writeFile "trunTransformedClear.txt" $ unlines $ map show $ actualRunCode

    -- assertEqual "Deployed bytecode is as expected" ("0x" <> bsEncodedRunTime) code
    let testValueA = "0000000000000000000000000000000000000000000000000000000000000045"
        testValueB = "0000000000000000000000000000000000000000000000000000000000000003"
        testValueRes = "0000000000000000000000000000000000000000000000000000000000000048"
    resRaw <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "add" False
                    [ FunctionArg "a" "uint256"
                    , FunctionArg "b" "uint256"
                    ] (Just [FunctionArg "" "uint256"]))) <> testValueA <> testValueB)
            })
        theCall <- Eth.call details Latest
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    case resRaw of
        Left e -> assertFailure $ show e
        Right res -> assertEqual "Result" ("0x" <> testValueRes) res

storeAndGetOnChainProtected = TestLabel "\"StorerAndGetter\" on chain (protected, in bound)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    bsEncodedFull <- compileSolidityFileBinFull "test/Models/StorerAndGetter.sol"
    bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/StorerAndGetter.sol"
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
    bytecodeRuntimeRes <- Control.Exception.try $ parseGoodExample bsDecodedRunTime :: IO (Either SomeException [OpCode])
    case bytecodeRuntimeRes of
        Left e -> error $ "abc" ++ show e
        Right bytecodeRuntime -> do
            writeFile "trun.txt" $ unlines $ map show $ countCodes bytecodeRuntime
            if not $ null $ checkStaticJumps bytecodeRuntime
                then assertFailure $ "Static jumps not ok before transform (runtime) " ++ show (checkStaticJumps bytecode)
                else pure ()
    writeFile "tbuildTransformedPre.txt" $ unlines $ map show $ countCodes $ bytecode
    writeFile "tbuildTransformedMid.txt" $ unlines $ map show $ (\(_,_,c)->c) $ midTransform bytecode
    writeFile "tbuildTransformed.txt" $ unlines $ map show $ countCodes $ transform bytecode
    -- mapM_ print (countCodes bytecode)
    writeFile "t.txt" $ unlines $ map show $ countCodes bytecode
    writeFile "ttrans.txt" $ unlines $ map show $ countCodes $ transform bytecode

    -- if not $ null $ checkStaticJumps bytecode
    --     then assertFailure $ "Static jumps not ok before transform " ++ show (checkStaticJumps bytecode)
    --     else pure ()
    let bsEncoded = B16.encode $ B.concat $ map toByteString $ transform bytecode
    -- if not $ null $ checkStaticJumps bytecode
    --     then assertFailure $ "Static jumps not ok after transform " ++ show (checkStaticJumps $ transform bytecode)
    --     else pure ()
    (Right availableAccounts) <- runWeb3 accounts
    let sender = availableAccounts !! 1
    deployRes <- Control.Exception.try $ deployContract sender bsEncoded :: IO (Either SomeException (Text, Text))
    let (res, tx) = case deployRes of
            Right x -> x
            Left e -> error ("123" ++ show e)
    newContractAddress <- getContractAddress tx

    codeRaw <- runWeb3 $ getCode newContractAddress Latest
    let code = case codeRaw of
            Left e -> error $ "sdasdas" ++  show e
            Right x -> x

    r <- Control.Exception.try $ parseGoodExample $ fst $ B16.decode {- $ B.drop ((17+3+9+5)*2) -} $ B.drop 2 $ encodeUtf8 code :: IO (Either SomeException [OpCode])
    let actualRunCode = case r of
            Right x -> x
            Left e -> error ("rrrt" ++ show e)
    writeFile "trunTransformed.txt" $ unlines $ map show $ countCodes $ actualRunCode
    writeFile "trunTransformedClear.txt" $ unlines $ map show $ actualRunCode

    -- testJumps actualRunCode

    let testValue = "0000000000000000000000000000000000000000000000000000000000000045"
    -- Use a call (send a transaction) to "store" to set a particular value
    storeResRaw <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "store" False
                    [ FunctionArg "loo" "uint256"
                    ] Nothing)) <> testValue)
            })
        theCall <- Eth.sendTransaction details
        pure (theCall)
    case storeResRaw of
        Right storeRes -> assertBool "Store Result" ("0x0" /= storeRes)
        Left e -> error $ "ssssseee" ++ show e
    --  Use a call to "get" to ensure that the stored value has been correctly set.
    getResRaw <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "get" True
                    [] (Just [FunctionArg "d" "uint256"]))))
            })
        theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    getRes <- case getResRaw of
        Left e -> assertFailure $  ":LK " ++ show e
        Right x -> pure x
    assertEqual "Result" ("0x" <> testValue) getRes

storeAndGetOnChainUnprotected  = TestLabel "\"StorerAndGetter\" on chain (unprotected, in bound)" $ TestCase $ do
    -- Read in the test Solidity source file. This file contains a
    -- Solidity contract with a single unprotected SSTORE call.
    bsEncodedFull <- compileSolidityFileBinFull "test/Models/StorerAndGetter.sol"
    bsEncodedFull2 <- compileSolidityFileBinFull "test/Models/StorerAndGetter.sol"
    bsEncodedRunTime <- compileSolidityFileBinRunTime "test/Models/StorerAndGetter.sol"
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    let bsEncoded = B16.encode bsDecodedFull
    (Right availableAccounts) <- runWeb3 accounts
    let sender = availableAccounts !! 1
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx

    buildCode <- parseGoodExample bsDecodedFull
    writeFile "tbuildUntransformed.txt"  $ unlines $ map show $ countCodes buildCode

    (Right code) <- runWeb3 $ getCode newContractAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
    writeFile "trunUntransformed.txt" $ unlines $ map show $ countCodes actualRunCode
    writeFile "trunUntransformedClear.txt" $ unlines $ map show $ actualRunCode

    -- testJumps code
    testJumps actualRunCode
    let testValue = "0000000000000000000000000000000000000000000000000000000000000045"
    -- Use a call (send a transaction) to "store" to set a particular value
    (Right (storeRes)) <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "store" False
                    [ FunctionArg "loo" "uint256"
                    ] Nothing)) <> testValue)
            })
        theCall <- Eth.sendTransaction details
        pure (theCall)
    assertBool "Store Result" ("0x0" /= storeRes)
    --  Use a call to "get" to ensure that the stored value has been correctly set.
    (Right (getRes)) <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Just newContractAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "get" False
                    [] (Just [FunctionArg "d" "uint256"]))))
            })
        theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
        -- theCall <- Eth.sendTransaction details
        pure (theCall)
    assertEqual "Result" ("0x" <> testValue) getRes

deployContract sender bsEncoded =  do
    r <- runWeb3 $ do
        let details = (Call {
                callFrom = Just sender,
                callTo = Nothing,
                callGas = Just 900000,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just (T.pack $ C8.unpack $ "0x" `B.append` bsEncoded)
            })
        theCall <- Eth.call details Latest
        theEffect <- Eth.sendTransaction details
        pure (theCall, theEffect)
    case r of
        Left e -> assertFailure $ show e
        Right x -> pure x

getContractAddress tx = do
    contractAddressResult <- runWeb3 $ do
        r <- getTransactionReceipt tx
        pure $ txrContractAddress r
    case contractAddressResult of
            Left e -> assertFailure ("ss" ++ show e)
            Right (Just x) -> pure x
            Right Nothing -> assertFailure "No new contract address was returned"
