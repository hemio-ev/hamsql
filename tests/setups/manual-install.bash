#!/bin/bash

../../dist/build/hamsql/hamsql install -d --permit-data-deletion -v -c postgres://postgres@/hamsql-test -s $1
