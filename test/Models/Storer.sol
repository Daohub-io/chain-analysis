pragma solidity ^0.4.17;

contract Storer {
    function store() public {
        uint256 loo = 1234;
        assembly {
            mload(loo)
            0x0100000100000000000000000000000000000000000000000000000000000000
            sstore
        }
    }
}
