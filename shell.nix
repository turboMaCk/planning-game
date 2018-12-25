let
  pkgs = import <nixpkgs> { };

  addons =
    pkgs.mkShell {
      buildInputs = with pkgs;
        [ elmPackages.elm
          haskellPackages.ghcid
        ];
    };
in
(import ./default.nix).server.env // addons
