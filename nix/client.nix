{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
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
          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --output $out/public/app.js --docs $out/share/doc/${module}.json --optimize
        '') targets)}
      '';
    };
in mkDerivation {
  name = "elm-app-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ../.;
  targets = ["Main"];
  srcdir = "./client";
}
