#!/bin/bash -e

database="postgres://postgres@localhost/test1"
params="-s carnivora/setup.yaml $@"

function run() {
    echo "# $@"
    $@
}

run ghc -W Main
run ./Main ${params} -d > out/documentation.html
run ./Main ${params} -g > out/documentation.dot
run ./Main ${params} -p > out/installation.sql
run ./Main ${params} -j > out/installation.json
tail out/documentation.html out/documentation.dot out/installation.sql
run ./Main ${params} -e -c${database}

