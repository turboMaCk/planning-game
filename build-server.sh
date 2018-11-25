# /usr/env bash

cabal build --builddir=dist

ln -s ./dist/build/agile-pocker/agile-pocker ./serve
