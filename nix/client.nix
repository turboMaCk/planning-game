{ pkgs }:
with pkgs;
let
  nodePackages = import ./node-composition.nix {};

  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm nodePackages.uglify-js ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        inherit versionsDat;
      };

      installPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir -p $out/share/doc
        cp -r public $out/public
        ${lib.concatStrings (map (module: ''
          js="app.js"
          min=$out/"public/app.js"

          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --output $js --docs $out/share/doc/${module}.json --optimize

          uglifyjs $js \
            --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | uglifyjs --mangle --output=$min

          echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
          echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
          echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

          uglifyjs ${srcdir}/init.js --compress | uglifyjs --mangle --output=$out/public/init.js

        '') targets)}
      '';
    };
in mkDerivation {
  name = "planning-game-client-0.3.0";
  srcs = ./elm-srcs.nix;
  src = ../.;
  targets = ["Main"];
  srcdir = "./client";
}
