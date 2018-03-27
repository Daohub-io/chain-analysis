# evm-mod

This is configured to build using [stack](https://docs.haskellstack.org/en/stable/README/).

## Bulding:

```
stack build
```

## Testing:

In order to run the tests, you will need to install a solidity compiler. It is
currently configured to use the solcjs compiler from npm.

```
npm i -D solc
stack test
```
