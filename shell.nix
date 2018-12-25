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
(import ./build.nix).server.env // addons