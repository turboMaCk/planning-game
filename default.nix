let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          agilePoker =
            haskellPackagesNew.callPackage ./server.nix {
              libiconv = pkgs.libiconv;
            };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

  frontend =
    pkgs.callPackage ./client.nix {
      stdenv = pkgs.stdenv;
      elm = pkgs.elmPackages.elm;
      server = pkgs.haskellPackages.agilePoker;
    };
in
rec { server = pkgs.haskellPackages.agilePoker;
      client = frontend;
    }
