#!/bin/bash -e

database="postgres://postgres@localhost/test1"
params="-s carnivora/setup.yaml"

ghc Main
./Main ${params} -d > out/documentation.html
./Main ${params} -p > out/installation.sql
./Main ${params} -j > out/installation.json
tail out/documentation.html out/installation.sql
./Main ${params} -e -c${database}

