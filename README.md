HamSql
======

Interpreter for SQL-structure definitions in Yaml (YamSql)

[![build status](https://git.hemio.de/hemio/hamsql/badges/master/build.svg)](https://git.hemio.de/hemio/hamsql/commits/master)
[![Build Status](https://travis-ci.org/qua-bla/hamsql.svg?branch=master)](https://travis-ci.org/qua-bla/hamsql)

## Building HamSql on Debian

Install haskell compiler and required libraries:

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

Now you can

    make
    make install
