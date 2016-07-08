HamSql
======

Interpreter for SQL-structure definitions in Yaml ([YamSql](http://yamsql.readthedocs.io/))

- [![build status](https://git.hemio.de/hemio/hamsql/badges/master/build.svg)](https://git.hemio.de/hemio/hamsql/commits/master) *Debian Stable*
- [![Build Status](https://travis-ci.org/qua-bla/hamsql.svg?branch=master)](https://travis-ci.org/qua-bla/hamsql) *GHC 7.6, 7.8*

## Building HamSql on Debian

Install haskell compiler and required libraries:

```sh
apt-get install \
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
