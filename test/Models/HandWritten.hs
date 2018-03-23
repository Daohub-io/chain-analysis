module Models.HandWritten where

import Data.ByteString

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