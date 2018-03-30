pragma solidity ^0.4.17;

contract StorerProtectedSubRoutine {
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
                swap1 // swap store address and continue address (store on top after this)
                0x0100000000000000000000000000000000000000000000000000000000000000 // lower limit
                dup2 // duplicate store address for comparison
                lt // see if address is lower than the lower limit
                0x0200000000000000000000000000000000000000000000000000000000000000 // upper limit
                dup3 // duplicate store address for comparison
                gt // see if the store address is higher than the upper limit
                or // set top of stack to 1 if either is true
                excAddress // push the exception address
                jumpi // jump to exception if SSTORE is out of pounds
                swap1 // to put the continue address
                swap2 //      under the two SSTORE arguments
                swap1 //
                sstore // perform the store
                jump // return to the call site and continue
            excAddress:
                0x0
                0x0
                revert
            storeProcEnd: // For fall-through protection
            end:
        }
    }
}
