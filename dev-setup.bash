#!/bin/bash


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
    libghc-hdbc-postgresql-dev

