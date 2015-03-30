#!/bin/bash -e

database="postgres://postgres@/test1"
params="-s carnivora/setup.yaml $@"
bin="./dist/build/hamsql/hamsql"

function run() {
    echo "# $@"
    $@
}

cabal build
run $bin doc ${params} -f html > out/documentation.html
run $bin doc ${params} -f dot > out/documentation.dot
run $bin install ${params} -c $database -p > out/installation.sql
#run $bin ${params} -j > out/installation.json
tail out/documentation.html out/documentation.dot out/installation.sql
run $bin install ${params} -c ${database}

