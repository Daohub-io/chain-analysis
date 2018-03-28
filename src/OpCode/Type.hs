module OpCode.Type where

import Prelude hiding (LT, EQ, GT)

import Control.Applicative
import Data.ByteString
import Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString as A
-- import Data.Attoparsec.Binary
import Data.Char (isSpace)
import Data.List as L
import Debug.Trace

import Data.Word
import Control.Monad

import Test.QuickCheck

import OpCode.Utils

data OpCode
    = STOP
    | ADD
    | MUL
    | SUB
    | DIV
    | SDIV
    | MOD
    | SMOD
    | ADDMOD
    | MULMOD
    | EXP
    | SIGNEXTEND
    | LT
    | GT
    | SLT
    | SGT
    | EQ
    | ISZERO
    | AND
    | OR
    | XOR
    | NOT
    | BYTE
    | SHA3
    | ADDRESS
    | BALANCE
    | ORIGIN
    | CALLER
    | CALLVALUE
    | CALLDATALOAD
    | CALLDATASIZE
    | CALLDATACOPY
    | CODESIZE
    | CODECOPY
    | GASPRICE
    | EXTCODESIZE
    | EXTCODECOPY
    | RETURNDATASIZE
    | RETURNDATACOPY
    | BLOCKHASH
    | COINBASE
    | TIMESTAMP
    | NUMBER
    | DIFFICULTY
    | GASLIMIT
    | POP
    | MLOAD
    | MSTORE
    | MSTORE8
    | SLOAD
    | SSTORE
    | JUMP
    | JUMPI
    | PC
    | MSIZE
    | GAS
    | JUMPDEST
    | PUSH1 ByteString
    | PUSH2 ByteString
    | PUSH3 ByteString
    | PUSH4 ByteString
    | PUSH5 ByteString
    | PUSH6 ByteString
    | PUSH7 ByteString
    | PUSH8 ByteString
    | PUSH9 ByteString
    | PUSH10 ByteString
    | PUSH11 ByteString
    | PUSH12 ByteString
    | PUSH13 ByteString
    | PUSH14 ByteString
    | PUSH15 ByteString
    | PUSH16 ByteString
    | PUSH17 ByteString
    | PUSH18 ByteString
    | PUSH19 ByteString
    | PUSH20 ByteString
    | PUSH21 ByteString
    | PUSH22 ByteString
    | PUSH23 ByteString
    | PUSH24 ByteString
    | PUSH25 ByteString
    | PUSH26 ByteString
    | PUSH27 ByteString
    | PUSH28 ByteString
    | PUSH29 ByteString
    | PUSH30 ByteString
    | PUSH31 ByteString
    | PUSH32 ByteString
    | DUP1
    | DUP2
    | DUP3
    | DUP4
    | DUP5
    | DUP6
    | DUP7
    | DUP8
    | DUP9
    | DUP10
    | DUP11
    | DUP12
    | DUP13
    | DUP14
    | DUP15
    | DUP16
    | SWAP1
    | SWAP2
    | SWAP3
    | SWAP4
    | SWAP5
    | SWAP6
    | SWAP7
    | SWAP8
    | SWAP9
    | SWAP10
    | SWAP11
    | SWAP12
    | SWAP13
    | SWAP14
    | SWAP15
    | SWAP16
    | LOG0
    | LOG1
    | LOG2
    | LOG3
    | LOG4
    | CREATE
    | CALL
    | CALLCODE
    | RETURN
    | DELEGATECALL
    | STATICCALL
    | REVERT
    | INVALID
    | SELFDESTRUCT
    deriving (Eq)

instance Show OpCode where
    show STOP = "STOP"
    show ADD = "ADD"
    show MUL = "MUL"
    show SUB = "SUB"
    show DIV = "DIV"
    show SDIV = "SDIV"
    show MOD = "MOD"
    show SMOD = "SMOD"
    show ADDMOD = "ADDMOD"
    show MULMOD = "MULMOD"
    show EXP = "EXP"
    show SIGNEXTEND = "SIGNEXTEND"
    show LT = "LT"
    show GT = "GT"
    show SLT = "SLT"
    show SGT = "SGT"
    show EQ = "EQ"
    show ISZERO = "ISZERO"
    show AND = "AND"
    show OR = "OR"
    show XOR = "XOR"
    show NOT = "NOT"
    show BYTE = "BYTE"
    show SHA3 = "SHA3"
    show ADDRESS = "ADDRESS"
    show BALANCE = "BALANCE"
    show ORIGIN = "ORIGIN"
    show CALLER = "CALLER"
    show CALLVALUE = "CALLVALUE"
    show CALLDATALOAD = "CALLDATALOAD"
    show CALLDATASIZE = "CALLDATASIZE"
    show CALLDATACOPY = "CALLDATACOPY"
    show CODESIZE = "CODESIZE"
    show CODECOPY = "CODECOPY"
    show GASPRICE = "GASPRICE"
    show EXTCODESIZE = "EXTCODESIZE"
    show EXTCODECOPY = "EXTCODECOPY"
    show RETURNDATASIZE = "RETURNDATASIZE"
    show RETURNDATACOPY = "RETURNDATACOPY"
    show BLOCKHASH = "BLOCKHASH"
    show COINBASE = "COINBASE"
    show TIMESTAMP = "TIMESTAMP"
    show NUMBER = "NUMBER"
    show DIFFICULTY = "DIFFICULTY"
    show GASLIMIT = "GASLIMIT"
    show POP = "POP"
    show MLOAD = "MLOAD"
    show MSTORE = "MSTORE"
    show MSTORE8 = "MSTORE8"
    show SLOAD = "SLOAD"
    show SSTORE = "SSTORE"
    show JUMP = "JUMP"
    show JUMPI = "JUMPI"
    show PC = "PC"
    show MSIZE = "MSIZE"
    show GAS = "GAS"
    show JUMPDEST = "JUMPDEST"
    show (PUSH1 bytes) = "PUSH1 " ++ show (B16.encode bytes)
    show (PUSH2 bytes) = "PUSH2 " ++ show (B16.encode bytes)
    show (PUSH3 bytes) = "PUSH3 " ++ show (B16.encode bytes)
    show (PUSH4 bytes) = "PUSH4 " ++ show (B16.encode bytes)
    show (PUSH5 bytes) = "PUSH5 " ++ show (B16.encode bytes)
    show (PUSH6 bytes) = "PUSH6 " ++ show (B16.encode bytes)
    show (PUSH7 bytes) = "PUSH7 " ++ show (B16.encode bytes)
    show (PUSH8 bytes) = "PUSH8 " ++ show (B16.encode bytes)
    show (PUSH9 bytes) = "PUSH9 " ++ show (B16.encode bytes)
    show (PUSH10 bytes) = "PUSH10 " ++ show (B16.encode bytes)
    show (PUSH11 bytes) = "PUSH11 " ++ show (B16.encode bytes)
    show (PUSH12 bytes) = "PUSH12 " ++ show (B16.encode bytes)
    show (PUSH13 bytes) = "PUSH13 " ++ show (B16.encode bytes)
    show (PUSH14 bytes) = "PUSH14 " ++ show (B16.encode bytes)
    show (PUSH15 bytes) = "PUSH15 " ++ show (B16.encode bytes)
    show (PUSH16 bytes) = "PUSH16 " ++ show (B16.encode bytes)
    show (PUSH17 bytes) = "PUSH17 " ++ show (B16.encode bytes)
    show (PUSH18 bytes) = "PUSH18 " ++ show (B16.encode bytes)
    show (PUSH19 bytes) = "PUSH19 " ++ show (B16.encode bytes)
    show (PUSH20 bytes) = "PUSH20 " ++ show (B16.encode bytes)
    show (PUSH21 bytes) = "PUSH21 " ++ show (B16.encode bytes)
    show (PUSH22 bytes) = "PUSH22 " ++ show (B16.encode bytes)
    show (PUSH23 bytes) = "PUSH23 " ++ show (B16.encode bytes)
    show (PUSH24 bytes) = "PUSH24 " ++ show (B16.encode bytes)
    show (PUSH25 bytes) = "PUSH25 " ++ show (B16.encode bytes)
    show (PUSH26 bytes) = "PUSH26 " ++ show (B16.encode bytes)
    show (PUSH27 bytes) = "PUSH27 " ++ show (B16.encode bytes)
    show (PUSH28 bytes) = "PUSH28 " ++ show (B16.encode bytes)
    show (PUSH29 bytes) = "PUSH29 " ++ show (B16.encode bytes)
    show (PUSH30 bytes) = "PUSH30 " ++ show (B16.encode bytes)
    show (PUSH31 bytes) = "PUSH31 " ++ show (B16.encode bytes)
    show (PUSH32 bytes) = "PUSH32 " ++ show (B16.encode bytes)
    show DUP1 = "DUP1"
    show DUP2 = "DUP2"
    show DUP3 = "DUP3"
    show DUP4 = "DUP4"
    show DUP5 = "DUP5"
    show DUP6 = "DUP6"
    show DUP7 = "DUP7"
    show DUP8 = "DUP8"
    show DUP9 = "DUP9"
    show DUP10 = "DUP10"
    show DUP11 = "DUP11"
    show DUP12 = "DUP12"
    show DUP13 = "DUP13"
    show DUP14 = "DUP14"
    show DUP15 = "DUP15"
    show DUP16 = "DUP16"
    show SWAP1 = "SWAP1"
    show SWAP2 = "SWAP2"
    show SWAP3 = "SWAP3"
    show SWAP4 = "SWAP4"
    show SWAP5 = "SWAP5"
    show SWAP6 = "SWAP6"
    show SWAP7 = "SWAP7"
    show SWAP8 = "SWAP8"
    show SWAP9 = "SWAP9"
    show SWAP10 = "SWAP10"
    show SWAP11 = "SWAP11"
    show SWAP12 = "SWAP12"
    show SWAP13 = "SWAP13"
    show SWAP14 = "SWAP14"
    show SWAP15 = "SWAP15"
    show SWAP16 = "SWAP16"
    show LOG0 = "LOG0"
    show LOG1 = "LOG1"
    show LOG2 = "LOG2"
    show LOG3 = "LOG3"
    show LOG4 = "LOG4"
    show CREATE = "CREATE"
    show CALL = "CALL"
    show CALLCODE = "CALLCODE"
    show RETURN = "RETURN"
    show DELEGATECALL = "DELEGATECALL"
    show STATICCALL = "STATICCALL"
    show REVERT = "REVERT"
    show INVALID = "INVALID"
    show SELFDESTRUCT = "SELFDESTRUCT"

nBytes :: OpCode -> Integer
nBytes (PUSH1 _) = 1 + 1
nBytes (PUSH2 _) = 1 + 2
nBytes (PUSH3 _) = 1 + 3
nBytes (PUSH4 _) = 1 + 4
nBytes (PUSH5 _) = 1 + 5
nBytes (PUSH6 _) = 1 + 6
nBytes (PUSH7 _) = 1 + 7
nBytes (PUSH8 _) = 1 + 8
nBytes (PUSH9 _) = 1 + 9
nBytes (PUSH10 _) = 1 + 10
nBytes (PUSH11 _) = 1 + 11
nBytes (PUSH12 _) = 1 + 12
nBytes (PUSH13 _) = 1 + 13
nBytes (PUSH14 _) = 1 + 14
nBytes (PUSH15 _) = 1 + 15
nBytes (PUSH16 _) = 1 + 16
nBytes (PUSH17 _) = 1 + 17
nBytes (PUSH18 _) = 1 + 18
nBytes (PUSH19 _) = 1 + 19
nBytes (PUSH20 _) = 1 + 20
nBytes (PUSH21 _) = 1 + 21
nBytes (PUSH22 _) = 1 + 22
nBytes (PUSH23 _) = 1 + 23
nBytes (PUSH24 _) = 1 + 24
nBytes (PUSH25 _) = 1 + 25
nBytes (PUSH26 _) = 1 + 26
nBytes (PUSH27 _) = 1 + 27
nBytes (PUSH28 _) = 1 + 28
nBytes (PUSH29 _) = 1 + 29
nBytes (PUSH30 _) = 1 + 30
nBytes (PUSH31 _) = 1 + 31
nBytes (PUSH32 _) = 1 + 32
nBytes _ = 1

instance Arbitrary OpCode where
    arbitrary = oneof
        [ pure STOP
        , pure ADD
        , pure MUL
        , pure SUB
        , pure DIV
        , pure SDIV
        , pure MOD
        , pure SMOD
        , pure ADDMOD
        , pure MULMOD
        , pure EXP
        , pure SIGNEXTEND
        , pure LT
        , pure GT
        , pure SLT
        , pure SGT
        , pure EQ
        , pure ISZERO
        , pure AND
        , pure OR
        , pure XOR
        , pure NOT
        , pure BYTE
        , pure SHA3
        , pure ADDRESS
        , pure BALANCE
        , pure ORIGIN
        , pure CALLER
        , pure CALLVALUE
        , pure CALLDATALOAD
        , pure CALLDATASIZE
        , pure CALLDATACOPY
        , pure CODESIZE
        , pure CODECOPY
        , pure GASPRICE
        , pure EXTCODESIZE
        , pure EXTCODECOPY
        , pure RETURNDATASIZE
        , pure RETURNDATACOPY
        , pure BLOCKHASH
        , pure COINBASE
        , pure TIMESTAMP
        , pure NUMBER
        , pure DIFFICULTY
        , pure GASLIMIT
        , pure POP
        , pure MLOAD
        , pure MSTORE
        , pure MSTORE8
        , pure SLOAD
        , pure SSTORE
        , pure JUMP
        , pure JUMPI
        , pure PC
        , pure MSIZE
        , pure GAS
        , pure JUMPDEST
        , arbitraryPUSH1
        , arbitraryPUSH2
        , arbitraryPUSH3
        , arbitraryPUSH4
        , arbitraryPUSH5
        , arbitraryPUSH6
        , arbitraryPUSH7
        , arbitraryPUSH8
        , arbitraryPUSH9
        , arbitraryPUSH10
        , arbitraryPUSH11
        , arbitraryPUSH12
        , arbitraryPUSH13
        , arbitraryPUSH14
        , arbitraryPUSH15
        , arbitraryPUSH16
        , arbitraryPUSH17
        , arbitraryPUSH18
        , arbitraryPUSH19
        , arbitraryPUSH20
        , arbitraryPUSH21
        , arbitraryPUSH22
        , arbitraryPUSH23
        , arbitraryPUSH24
        , arbitraryPUSH25
        , arbitraryPUSH26
        , arbitraryPUSH27
        , arbitraryPUSH28
        , arbitraryPUSH29
        , arbitraryPUSH30
        , arbitraryPUSH31
        , arbitraryPUSH32
        , pure DUP1
        , pure DUP2
        , pure DUP3
        , pure DUP4
        , pure DUP5
        , pure DUP6
        , pure DUP7
        , pure DUP8
        , pure DUP9
        , pure DUP10
        , pure DUP11
        , pure DUP12
        , pure DUP13
        , pure DUP14
        , pure DUP15
        , pure DUP16
        , pure SWAP1
        , pure SWAP2
        , pure SWAP3
        , pure SWAP4
        , pure SWAP5
        , pure SWAP6
        , pure SWAP7
        , pure SWAP8
        , pure SWAP9
        , pure SWAP10
        , pure SWAP11
        , pure SWAP12
        , pure SWAP13
        , pure SWAP14
        , pure SWAP15
        , pure SWAP16
        , pure LOG0
        , pure LOG1
        , pure LOG2
        , pure LOG3
        , pure LOG4
        , pure CREATE
        , pure CALL
        , pure CALLCODE
        , pure RETURN
        , pure DELEGATECALL
        , pure STATICCALL
        , pure REVERT
        , pure INVALID
        , pure SELFDESTRUCT
        ]

arbitraryPUSH1  = PUSH1  <$> (fmap pack $ replicateM 1 arbitrary)
arbitraryPUSH2  = PUSH2  <$> (fmap pack $ replicateM  2 arbitrary)
arbitraryPUSH3  = PUSH3  <$> (fmap pack $ replicateM  3 arbitrary)
arbitraryPUSH4  = PUSH4  <$> (fmap pack $ replicateM  4 arbitrary)
arbitraryPUSH5  = PUSH5  <$> (fmap pack $ replicateM  5 arbitrary)
arbitraryPUSH6  = PUSH6  <$> (fmap pack $ replicateM  6 arbitrary)
arbitraryPUSH7  = PUSH7  <$> (fmap pack $ replicateM  7 arbitrary)
arbitraryPUSH8  = PUSH8  <$> (fmap pack $ replicateM  8 arbitrary)
arbitraryPUSH9  = PUSH9  <$> (fmap pack $ replicateM  9 arbitrary)
arbitraryPUSH10 = PUSH10 <$> (fmap pack $ replicateM 10 arbitrary)
arbitraryPUSH11 = PUSH11 <$> (fmap pack $ replicateM 11 arbitrary)
arbitraryPUSH12 = PUSH12 <$> (fmap pack $ replicateM 12 arbitrary)
arbitraryPUSH13 = PUSH13 <$> (fmap pack $ replicateM 13 arbitrary)
arbitraryPUSH14 = PUSH14 <$> (fmap pack $ replicateM 14 arbitrary)
arbitraryPUSH15 = PUSH15 <$> (fmap pack $ replicateM 15 arbitrary)
arbitraryPUSH16 = PUSH16 <$> (fmap pack $ replicateM 16 arbitrary)
arbitraryPUSH17 = PUSH17 <$> (fmap pack $ replicateM 17 arbitrary)
arbitraryPUSH18 = PUSH18 <$> (fmap pack $ replicateM 18 arbitrary)
arbitraryPUSH19 = PUSH19 <$> (fmap pack $ replicateM 19 arbitrary)
arbitraryPUSH20 = PUSH20 <$> (fmap pack $ replicateM 20 arbitrary)
arbitraryPUSH21 = PUSH21 <$> (fmap pack $ replicateM 21 arbitrary)
arbitraryPUSH22 = PUSH22 <$> (fmap pack $ replicateM 22 arbitrary)
arbitraryPUSH23 = PUSH23 <$> (fmap pack $ replicateM 23 arbitrary)
arbitraryPUSH24 = PUSH24 <$> (fmap pack $ replicateM 24 arbitrary)
arbitraryPUSH25 = PUSH25 <$> (fmap pack $ replicateM 25 arbitrary)
arbitraryPUSH26 = PUSH26 <$> (fmap pack $ replicateM 26 arbitrary)
arbitraryPUSH27 = PUSH27 <$> (fmap pack $ replicateM 27 arbitrary)
arbitraryPUSH28 = PUSH28 <$> (fmap pack $ replicateM 28 arbitrary)
arbitraryPUSH29 = PUSH29 <$> (fmap pack $ replicateM 29 arbitrary)
arbitraryPUSH30 = PUSH30 <$> (fmap pack $ replicateM 30 arbitrary)
arbitraryPUSH31 = PUSH31 <$> (fmap pack $ replicateM 31 arbitrary)
arbitraryPUSH32 = PUSH32 <$> (fmap pack $ replicateM 32 arbitrary)
