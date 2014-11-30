#!/bin/bash -e

database="postgres://postgres@localhost/test1"
params="-s carnivora/setup.yaml"

ghc -W Main
./Main ${params} -d > out/documentation.html
./Main ${params} -g > out/documentation.dot
./Main ${params} -p > out/installation.sql
./Main ${params} -j > out/installation.json
tail out/documentation.html out/documentation.dot out/installation.sql
./Main ${params} -e -c${database}

