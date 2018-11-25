# /usr/env bash

cabal build --builddir=dist

ln -sf ./dist/build/agile-poker/agile-poker ./serve
