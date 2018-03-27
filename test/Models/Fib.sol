pragma solidity ^0.4.17;

contract Fib {
    function fib() public {
        assembly {
                5
                1
                1
            start:
                dup3
                iszero
                end
                jumpi
                swap1
                dup2
                add
                swap2
                1
                swap1
                sub
                swap2
                start
                jump
            end:
                pop
                pop
                pop
                stop
        }
    }
}