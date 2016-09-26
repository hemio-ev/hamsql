#!/bin/bash

../../dist/build/hamsql/hamsql upgrade --permit-data-deletion -v -c postgres://postgres@/hamsql-test -s $1
