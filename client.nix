{ stdenv, elm, server }:

stdenv.mkDerivation {
  name = "agile-poker-client";
  src = ./.;

  buildInputs = [ elm ];

  buildPhase = ''
    export ELM_HOME=$(pwd)
    mkdir $out
    elm make client/Main.elm --output=app.js
  '';

  installPhase = ''
    mkdir $out/public
    cp public/* $out/public/*
    cp app.js $out/public/
  '';
}
