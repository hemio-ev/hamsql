hamsql
======

Interpreter for SQL-structure definitions in Yaml (YamSql)

[![Build Status](https://drone.io/github.com/qua-bla/hamsql/status.png)](https://drone.io/github.com/qua-bla/hamsql/latest)

## Getting started with developement

Install required libraries

> apt-get install haskell-platform libghc-yaml-dev libghc-missingh-dev libghc-pandoc-dev libghc-hdbc-postgresql-dev

With

> ./dev-setup.bash

you can install one remaining custom lib and check the install status of the above ones.

Compile with

> cabal sandbox init
> cabal install --only-dependencies
> cabal build

or

> ghc -fforce-recomp -Wall Main

or interactive: `ghci` and then `:l Parser.hs` or any other file.

