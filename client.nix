{ stdenv, elm }:

stdenv.mkDerivation {
  name = "agile-poker-client";
  src = ./.;

  buildInputs = [ elm ];

  buildPhase = ''
    export ELM_HOME=$(pwd)
    elm make client/Main.elm --output=app.js
  '';

  installPhase = ''
    mkdir $out
    cp -r public $out
    cp app.js $out/public/
  '';
}
