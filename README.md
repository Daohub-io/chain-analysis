# Chain-analysis

This is configured to build using [stack](https://docs.haskellstack.org/en/stable/README/).

## Bulding:

```
stack build
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