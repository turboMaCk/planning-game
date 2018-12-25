let
  config = {
    packageOverrides = pkgs: rec {
      client =
        pkgs.callPackage ./client.nix {
          stdenv = pkgs.stdenv;
          elm = pkgs.elmPackages.elm;
          server = pkgs.haskellPackages.agilePoker;
        };

      # Build docker container
      # Using staticaly liked version
      # This makes a huge difference in container size
      #    - Dynamically linked build container: 664M
      #    - Statically linked build container: 12M
      docker-container =
        pkgs.dockerTools.buildImage {
          name = "agile-poker";
          config.Cmd = [ "${haskellPackages.agilePoker-mini}/bin/agile-poker" ];
        };

      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            agilePoker =
              haskellPackagesNew.callPackage ./server.nix {
                libiconv = pkgs.libiconv;
                client = client;
              };

            # statically liked version (used for docker)
            agilePoker-mini =
              pkgs.haskell.lib.justStaticExecutables
                (haskellPackagesNew.callPackage ./server.nix {
                  libiconv = pkgs.libiconv;
                  client = client;
                });
          };
        };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };
in
rec {
  server = pkgs.haskellPackages.agilePoker;
  client = pkgs.client;
  docker = pkgs.docker-container;
}
