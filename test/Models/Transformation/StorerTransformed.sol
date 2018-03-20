pragma solidity ^0.4.17;

contract StorerTransformed {
    function store() public {
        uint256 loo = 1234;
        assembly {
            mload(loo)
            0x0100000100000000000000000000000000000000000000000000000000000000
            // This code is necessary in front of an SSTORE to pass verification
            0x64
            mload(0x40)
            mstore
            sstore
        }
    }
}
