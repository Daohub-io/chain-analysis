module Models.HandWritten where

import Data.ByteString

import OpCode.Type
import OpCode.Utils

hwsPass =
    [ hwP1
    ]

hwsFail =
    [ hwF1
    ]

hwP1 = pack
    [ 0x60 -- PUSH1
    , 0x43 -- a value
    , 0x50 -- POP
    ]

hwF1 = pack
    [ 0x1b -- not a valid opcode
    , 0x43 -- a value
    , 0x50 -- POP
    ]

protectedStore1 =
    [ PUSH32 $ integerToEVM256 0x0100000000000000000000000000000000000000000000000000000000000000 -- lower limit
    , DUP2 -- duplicate store address for comparison
    , OpCode.Type.LT -- see if address is lower than the lower limit
    , PUSH32 $ integerToEVM256 0x0200000000000000000000000000000000000000000000000000000000000000 -- upper limit
    , DUP3 -- duplicate store address for comparison
    , OpCode.Type.GT -- see if the store address is higher than the upper limit
    , OR -- set top of stack to 1 if either is true
    , PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
    , JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
    , SSTORE -- perform the store
    ]

storeUnprotected1 = [PUSH1 (pack [0x4]), PUSH1 (pack [0x0]), SSTORE]