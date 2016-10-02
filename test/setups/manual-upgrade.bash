#!/bin/bash

../../dist/build/hamsql/hamsql upgrade --permit-data-deletion -v \
    -c postgres://postgres@/hamsql-test \
    --sql-log ../tmp/upgrade.sql \
    -s $1
