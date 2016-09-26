#!/bin/bash

../../dist/build/hamsql/hamsql $1 -v -c postgres://postgres@/hamsql-test -s $2
