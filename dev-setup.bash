#!/bin/bash

if [ ! -e carnivora/setup.yaml ]; then
    git clone https://github.com/qua-bla/carnivora
else
    echo "Carnivora as example for tests is installed"
fi

if [ ! -e url/Network/URL.hs ]; then
    git clone https://github.com/qua-bla/url.git
else
    echo "Forked Network.URL lib seems to be installed"
fi

echo "List of required packages and their state"

dpkg-query -l \
    haskell-platform \
    libghc-yaml-dev \
    libghc-missingh-dev \
    libghc-pandoc-dev \
    haskell-postgresql-simple \
    optparse-applicative

