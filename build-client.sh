# /usr/env bash

cp client/init.js public/init.js
elm make client/Main.elm --output=public/Main.min.js
