HamSql
======

Interpreter for SQL-structure definitions in Yaml (YamSql)

[![Build Status](https://travis-ci.org/qua-bla/hamsql.svg?branch=master)](https://travis-ci.org/qua-bla/hamsql)

## Building HamSql

    cabal update
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    
Note: The build should work but the software will not work without the patched 
Url package (see `./dev-setup.bash` below!)

## Building HamSql on Debian

You can install some of the required libraries from debian packages (jessie)

    apt-get install haskell-platform libghc-yaml-dev libghc-missingh-dev libghc-pandoc-dev libghc-hdbc-postgresql-dev

With

    ./dev-setup.bash

you can install one remaining custom lib and check the install status of the above ones.

Finally you need cabal (~500 MB)

    apt-get install cabal-install

Now you can

    cabal update
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
