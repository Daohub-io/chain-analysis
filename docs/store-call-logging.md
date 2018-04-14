# Logging of Storge Calls (On-Chain)

In order to track the changes to storage of over time (over transactions), logs are inserted at each SSTORE call. There are 5 `LOG` opcodes that append a log to a number of topics.

- `LOG0` - Log to no topics.
- `LOG1` - Log to 1 topic.
- `LOG2` - Log to 2 topics.
- `LOG3` - Log to 3 topics.
- `LOG4` - Log to 4 topics.

`LOG0` expects there to be 2 arguments on the stack. These two arguments are:

```
top -> [0] start of memory buffer
       [1] length of memory buffer
```

For each of the other `LOGN` calls an additional `N` stack items are expected
below these items to define the topics to which this log will be published. For
example: a `LOG4` call expects the stack to be as below:

```
top -> [0] start of memory buffer
       [1] length of memory buffer
       [2] topic #1
       [3] topic #2
       [4] topic #3
       [5] topic #4
```

Note the use of memory buffer arguments. Any call to `LOG` requires using memory to pass the data. As we are transforming programs of unknown memory layout, and of an unknown memory allocation scheme, we are unable to allocate out own memory in the general case. We cannot even assume that the Solidity memory allocator is present.

To get around this problem we write to arbitrary points in memory to create a buffer for out `LOG` call. Before we write to this buffer, we load the values onto the stack (using two stack spaces). After we are finished with this buffer, we can write those values back into those memory locations from the stack, returning it to its original state. We will arbitrarily use the addresses 0x60 and 0x80 (which are contiguous at 32 bytes) for our `LOG` calls.

We want to store two pieces of information:

- The contract address (20 bytes).
- The storage key (32 bytes).

When consuming this information we would be interested in the procedure id and the capabilities of that procedure, but that information is not available at preprocessing time. Instead, with the contract address we should be able to determine these properties later. The data is therefore a fixed length of 50 bytes. The first 20 of which is the contract address, and the next 32 of which is the storage key with which `SSTORE` is being called.

This is the general procedure we want to follow for each logging call (this does not cover the storage call or its protection, simply the logging):

1. Load the two values at memory addresses 0x60 and 0x80 onto the stack so that they can be restored later.
2. Store the contract address at 0x60 in memory.
3. Store the storage key at 0x80 in memory. This needs to be taken at runtime, so we must assume it is on top of the stack when this routine runs.
4. Push the designated topic onto the stack.
5. Push the length of the memory buffer onto the stack. While we have allocated two 32-byte slots (64 bytes), we actually only need 52, so we push 52 (0x34) onto the stack.
6. Push the memory location of the start of the buffer. While the start of out buffer is at 0x60, the first 12 bytes are not part of the address (the address only occupies the lower 20 bytes), therefore this address is 0x60 + 12(0xc) = 0x6c.
7. Restore the original memory locations.

Translated into opcodes:

```haskell
-- Load the original values of our memory buffer onto the stack.
PUSH1 (pack [0x60])
MLOAD
PUSH1 (pack [0x80])
MLOAD

-- Load the contract address onto the stack, then store it at memory
-- location 0x60.
ADDRESS
PUSH1 (pack [0x60])
MSTORE

-- Take the storage key from the stack and store it at 0x80. Note that it is
-- in the 3rd position (beneath the two original memory values we just
-- loaded). Therefore we must swap it to the top of the stack. This has a
-- side effect in that it reverses the order of the two original memory
-- values. Rather than swap them back, we simply account for that later.
SWAP2
PUSH1 (pack [0x80])
MSTORE

-- Push the topic to which we publish to the stack. (NB: this is not defined
-- here).
PUSH32 topic
PUSH1 (pack [0x34])
PUSH1 (pack [0x6c])

-- Perform the LOG.
LOG1

-- Restore the original memory values. Remember that the order of these is
-- reversed by the SWAP2 used above, there we call MSTORE in the same order
-- we called MLOAD.
PUSH1 (pack [0x60])
MSTORE
PUSH1 (pack [0x80])
MSTORE
```
