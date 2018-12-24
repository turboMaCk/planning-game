let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          agilePoker =
            haskellPackagesNew.callPackage ./default.nix {
              elm = pkgs.elmPackages.elm;
              libiconv = pkgs.libiconv;
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
haskellPackages.agilePoker;
