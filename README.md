# beaker-preprocessor

[![CircleCI](https://circleci.com/gh/Daolab/beaker-preprocessor.svg?style=svg&circle-token=94c1ada8b1bd409ae2f7355cb4c76d4082cc1ad9)](https://circleci.com/gh/Daolab/beaker-preprocessor)

This is configured to build using [stack](https://docs.haskellstack.org/en/stable/README/).

## Bulding:

```
stack build
```

## Testing:

In order to run the tests, you will need to install a solidity compiler. It is
currently configured to use the solcjs compiler from npm. You will also need a
locally running test network to run on-chain tests. Ganache works well for this.

```
npm i -D solc
npm i ganache-cli
./node_modules/.bin/ganache-cli
stack test
```

## Documentation:

In order to produce documentation for the Haskell code run:

```
stack haddock --open .
```

## Scraping Chain Data

Run an Ethereum node. First establish an Ethereum node up to a minimum of
1,500,000 blocks using:

```
net-parity.bat
```

Then run in offline mode (lower resource usage) using:

```
net-parity-offline.bat
```

*NB:* Both of these will need to be modified to set a storage location for your
Ethereum data.