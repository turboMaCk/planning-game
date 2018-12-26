{ pkgs ? import <nixpkgs> { } }:
(pkgs.stdenv.mkDerivation {
  name = "agile-poker-shell";
  buildInputs = with pkgs;
    [ elmPackages.elm
      haskellPackages.ghcid
      (import ./default.nix).server
    ];
})

