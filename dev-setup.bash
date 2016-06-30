#!/bin/bash

echo "List of required packages and their state"

dpkg-query -l \
    ghc \
    cabal-install \
    libghc-aeson-dev \
    libghc-file-embed-dev \
    libghc-network-uri-dev \
    libghc-optparse-applicative-dev \
    libghc-pandoc-dev \
    libghc-postgresql-simple-dev \
    libghc-text-dev \
    libghc-unordered-containers-dev \
    libghc-yaml-dev \

