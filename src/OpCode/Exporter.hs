module OpCode.Exporter where

import OpCode.Type

import Prelude hiding (LT, EQ, GT)

import Control.Applicative
import Data.ByteString
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec.ByteString as A
-- import Data.Attoparsec.Binary
import Data.Char (isSpace)
import Data.List as L
import Debug.Trace

import Data.Word
import Control.Monad

import Test.QuickCheck

toByteString :: OpCode -> ByteString
toByteString STOP = pack [0x00]
toByteString ADD = pack [0x01]
toByteString MUL = pack [0x02]
toByteString SUB = pack [0x03]
toByteString DIV = pack [0x04]
toByteString SDIV = pack [0x05]
toByteString MOD = pack [0x06]
toByteString SMOD = pack [0x07]
toByteString ADDMOD = pack [0x08]
toByteString MULMOD = pack [0x09]
toByteString EXP = pack [0x0a]
toByteString SIGNEXTEND = pack [0x0b]

toByteString LT = pack [0x10]
toByteString GT = pack [0x11]
toByteString SLT = pack [0x12]
toByteString SGT = pack [0x13]
toByteString EQ = pack [0x14]
toByteString ISZERO = pack [0x15]
toByteString AND = pack [0x16]
toByteString OR = pack [0x17]
toByteString XOR = pack [0x18]
toByteString NOT = pack [0x19]
toByteString BYTE = pack [0x1a]

toByteString SHA3 = pack [0x20]

toByteString ADDRESS = pack [0x30]
toByteString BALANCE = pack [0x31]
toByteString ORIGIN = pack [0x32]
toByteString CALLER = pack [0x33]
toByteString CALLVALUE = pack [0x34]
toByteString CALLDATALOAD = pack [0x35]
toByteString CALLDATASIZE = pack [0x36]
toByteString CALLDATACOPY = pack [0x37]
toByteString CODESIZE = pack [0x38]
toByteString CODECOPY = pack [0x39]
toByteString GASPRICE = pack [0x3a]
toByteString EXTCODESIZE = pack [0x3b]
toByteString EXTCODECOPY = pack [0x3c]
toByteString RETURNDATASIZE = pack [0x3d]
toByteString RETURNDATACOPY = pack [0x3e]

toByteString BLOCKHASH = pack [0x40]
toByteString COINBASE = pack [0x41]
toByteString TIMESTAMP = pack [0x42]
toByteString NUMBER = pack [0x43]
toByteString DIFFICULTY = pack [0x44]
toByteString GASLIMIT = pack [0x45]

toByteString POP = pack [0x50]
toByteString MLOAD = pack [0x51]
toByteString MSTORE = pack [0x52]
toByteString MSTORE8 = pack [0x53]
toByteString SLOAD = pack [0x54]
toByteString SSTORE = pack [0x55]
toByteString JUMP = pack [0x56]
toByteString JUMPI = pack [0x57]
toByteString PC = pack [0x58]
toByteString MSIZE = pack [0x59]
toByteString GAS = pack [0x5a]
toByteString JUMPDEST = pack [0x5b]

toByteString (PUSH1 bs) = cons 0x60 bs
toByteString (PUSH2 bs) = cons 0x60 bs
toByteString (PUSH3 bs) = cons 0x60 bs
toByteString (PUSH4 bs) = cons 0x60 bs
toByteString (PUSH5 bs) = cons 0x60 bs
toByteString (PUSH6 bs) = cons 0x60 bs
toByteString (PUSH7 bs) = cons 0x60 bs
toByteString (PUSH8 bs) = cons 0x60 bs
toByteString (PUSH9 bs) = cons 0x60 bs
toByteString (PUSH10 bs) = cons 0x60 bs
toByteString (PUSH11 bs) = cons 0x60 bs
toByteString (PUSH12 bs) = cons 0x60 bs
toByteString (PUSH13 bs) = cons 0x60 bs
toByteString (PUSH14 bs) = cons 0x60 bs
toByteString (PUSH15 bs) = cons 0x60 bs
toByteString (PUSH16 bs) = cons 0x60 bs
toByteString (PUSH17 bs) = cons 0x60 bs
toByteString (PUSH18 bs) = cons 0x60 bs
toByteString (PUSH19 bs) = cons 0x60 bs
toByteString (PUSH20 bs) = cons 0x60 bs
toByteString (PUSH21 bs) = cons 0x60 bs
toByteString (PUSH22 bs) = cons 0x60 bs
toByteString (PUSH23 bs) = cons 0x60 bs
toByteString (PUSH24 bs) = cons 0x60 bs
toByteString (PUSH25 bs) = cons 0x60 bs
toByteString (PUSH26 bs) = cons 0x60 bs
toByteString (PUSH27 bs) = cons 0x60 bs
toByteString (PUSH28 bs) = cons 0x60 bs
toByteString (PUSH29 bs) = cons 0x60 bs
toByteString (PUSH30 bs) = cons 0x60 bs
toByteString (PUSH31 bs) = cons 0x60 bs
toByteString (PUSH32 bs) = cons 0x60 bs

toByteString DUP1 = pack [0x80]
toByteString DUP2 = pack [0x81]
toByteString DUP3 = pack [0x82]
toByteString DUP4 = pack [0x83]
toByteString DUP5 = pack [0x84]
toByteString DUP6 = pack [0x85]
toByteString DUP7 = pack [0x86]
toByteString DUP8 = pack [0x87]
toByteString DUP9 = pack [0x88]
toByteString DUP10 = pack [0x89]
toByteString DUP11 = pack [0x8a]
toByteString DUP12 = pack [0x8b]
toByteString DUP13 = pack [0x8c]
toByteString DUP14 = pack [0x8d]
toByteString DUP15 = pack [0x8e]
toByteString DUP16 = pack [0x8f]
toByteString SWAP1 = pack [0x90]
toByteString SWAP2 = pack [0x91]
toByteString SWAP3 = pack [0x92]
toByteString SWAP4 = pack [0x93]
toByteString SWAP5 = pack [0x94]
toByteString SWAP6 = pack [0x95]
toByteString SWAP7 = pack [0x96]
toByteString SWAP8 = pack [0x97]
toByteString SWAP9 = pack [0x98]
toByteString SWAP10 = pack [0x99]
toByteString SWAP11 = pack [0x9a]
toByteString SWAP12 = pack [0x9b]
toByteString SWAP13 = pack [0x9c]
toByteString SWAP14 = pack [0x9d]
toByteString SWAP15 = pack [0x9e]
toByteString SWAP16 = pack [0x9f]

toByteString LOG0 = pack [0xa0]
toByteString LOG1 = pack [0xa1]
toByteString LOG2 = pack [0xa2]
toByteString LOG3 = pack [0xa3]
toByteString LOG4 = pack [0xa4]

toByteString CREATE = pack [0xf0]
toByteString CALL = pack [0xf1]
toByteString CALLCODE = pack [0xf2]
toByteString RETURN = pack [0xf3]
toByteString DELEGATECALL = pack [0xf4]
toByteString STATICCALL = pack [0xfa]
toByteString REVERT = pack [0xfd]
toByteString INVALID = pack [0xfe]
toByteString SELFDESTRUCT = pack [0xff]
