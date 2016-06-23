#!/bin/bash

echo "List of required packages and their state"

dpkg-query -l \
    haskell-platform \
    libghc-optparse-applicative-dev \
    libghc-postgresql-simple-dev \
    libghc-yaml-dev

