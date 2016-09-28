HamSql
======

Interpreter for SQL-structure definitions in Yaml ([YamSql](http://yamsql.readthedocs.io/))

[![build status](https://git.hemio.de/hemio/hamsql/badges/master/build.svg)](https://git.hemio.de/hemio/hamsql/commits/master)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/hamsql.svg?maxAge=2592000)](https://hackage.haskell.org/package/hamsql)
[![Hackage](https://img.shields.io/hackage/v/hamsql.svg?maxAge=2592000)](https://hackage.haskell.org/package/hamsql)

## Building HamSql on Debian

Install haskell compiler and required libraries:

```sh
apt install \
 make \
 ghc \
 cabal-install \
 libghc-aeson-dev \
 libghc-file-embed-dev \
 libghc-network-uri-dev \
 libghc-optparse-applicative-dev \
 libghc-pandoc-dev \
 libghc-postgresql-simple-dev \
 libghc-text-dev \
 libghc-unordered-containers-dev \
 libghc-yaml-dev
```

Now you can

    make
    make install

Completly building from sources

    apt install make ghc cabal-install libpq-dev happy
    make

