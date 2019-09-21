{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

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

      buildInputs = [ elmPackages.elm ];

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
          js=$out/"app.js"
          min=$out/"public/app.js"

          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --output $js --docs $out/share/doc/${module}.json --optimize

          ${nodePackages.uglify-js}/bin/uglifyjs $js \
            --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | ${nodePackages.uglify-js}/bin/uglifyjs --mangle --output=$min

          echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
          echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
          echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"
        '') targets)}
      '';
    };
in mkDerivation {
  name = "planngin-game-client-0.3.0";
  srcs = ./elm-srcs.nix;
  src = ../.;
  targets = ["Main"];
  srcdir = "./client";
}
