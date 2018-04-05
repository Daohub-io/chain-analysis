pragma solidity ^0.4.17;

contract StorerAndGetter {
    function store(uint256 loo) public {
        assembly {
            loo
            0x0100000100000000000000000000000000000000000000000000000000000000
            sstore
        }
    }
    function get() public returns (uint256 d) {
        assembly {
            0x0100000100000000000000000000000000000000000000000000000000000000
            sload
            =: d
        }
    }
}
