pragma solidity ^0.4.17;

contract StorerProtected {
    function store() public {
        uint256 loo = 1234;
        uint256 lowerLimit = 0x0100000000000000000000000000000000000000000000000000000000000000;
        uint256 upperLimit = 0x0200000000000000000000000000000000000000000000000000000000000000;
        assembly {
                mload(loo) // value to store
                0x0100000100000000000000000000000000000000000000000000000000000000 // address to store
                // This code is necessary in front of an SSTORE to pass verification
                here
                storeProc
                jump
            here:
                jump(end)

            jump(storeProcEnd) // Rudimentary fall-through protection
            storeProc:
                swap1
                0x0100000000000000000000000000000000000000000000000000000000000000
                dup2
                lt
                0x0200000000000000000000000000000000000000000000000000000000000000
                dup3
                gt
                or
                excAddress
                jumpi
                swap1
                swap2
                swap1
                sstore
                jump
            excAddress:
                0x0
                0x0
                revert
            storeProcEnd: // For fall-through protection
            end:
        }
    }
}
