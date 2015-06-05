#!/bin/bash

if [ ! -e url/Network/URL.hs ]; then
    git clone https://github.com/qua-bla/url.git
else
    echo "Forked Network.URL lib seems to be installed"
fi

echo "List of required packages and their state"

dpkg-query -l \
    haskell-platform \
    libghc-missingh-dev \
    libghc-optparse-applicative-dev \
    libghc-pandoc-dev \
    libghc-postgresql-simple-dev \
    libghc-yaml-dev

