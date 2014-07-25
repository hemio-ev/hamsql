#!/bin/bash -e

database="postgres://postgres@localhost/test1"

ghc Main
./Main -d > out/documentation.html
./Main -p > out/installation.sql
./Main -j > out/installation.json
tail out/documentation.html out/installation.sql
./Main -e -c${database}

