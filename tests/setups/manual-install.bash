#!/bin/bash

../../dist/build/hamsql/hamsql install -d --permit-data-deletion -v \
    -c postgres://postgres@/hamsql-test \
    --sql-log ../tmp/install.sql \
    -s $1
