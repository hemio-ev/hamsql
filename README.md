hamsql
======

Interpreter for SQL-structure definitions in Yaml (YamSql)

[![Build Status](https://travis-ci.org/qua-bla/hamsql.svg?branch=master)](https://travis-ci.org/qua-bla/hamsql)

## Getting started with developement

Install required libraries

    apt-get install haskell-platform libghc-yaml-dev libghc-missingh-dev libghc-pandoc-dev libghc-hdbc-postgresql-dev

With

    ./dev-setup.bash

you can install one remaining custom lib and check the install status of the above ones.

Compile with

    cabal sandbox init
    cabal install --only-dependencies
    cabal build


