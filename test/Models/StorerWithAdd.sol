pragma solidity ^0.4.17;

contract StorerWithAdd {
    function storeWithAdd(uint256 a, uint256 b) public returns (uint256) {
        uint256 loo = 1234;
        assembly {
            mload(loo)
            0x0100000100000000000000000000000000000000000000000000000000000000
            sstore
        }
        return a + b;
    }
}
