HamSql
======

Interpreter for SQL-structure definitions in Yaml (YamSql)

[![Build Status](https://travis-ci.org/qua-bla/hamsql.svg?branch=master)](https://travis-ci.org/qua-bla/hamsql)

## Building HamSql on Debian

Install haskell compiler and required libraries:

    apt-get install haskell-platform make
    apt-get install libghc-missingh-dev libghc-optparse-applicative-dev \
                    libghc-pandoc-dev libghc-postgresql-simple-dev libghc-yaml-dev

Run the script

    ./dev-setup.bash

which installs a patched library and checks the install status of all libraries.
Now you can

    make
    make install
