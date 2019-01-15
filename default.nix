let
  config = {
    packageOverrides = pkgs: rec {
      client =
        pkgs.callPackage ./client.nix {
          stdenv = pkgs.stdenv;
          elm = pkgs.elmPackages.elm;
        };

      # Build docker container
      # Using staticaly liked version
      # This makes a huge difference in container size
      #    - Dynamically linked build container: 664M
      #    - Statically linked build container: 12M
      docker-container =
        pkgs.dockerTools.buildImage {
          name = "agile-poker";
          extraCommands = ''
            ln -s ${haskellPackages.agilePoker-mini}/bin/agile-poker ./agile-poker
          '';
          config.Cmd = [ "${haskellPackages.agilePoker-mini}/bin/agile-poker" "-qg" ];
        };

      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            agilePoker =
              haskellPackagesNew.callPackage ./server.nix {};

            # statically liked version (used for docker)
            agilePoker-mini =
              pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./server.nix {});
          };
        };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };
in with pkgs;
rec {
  server = haskellPackages.agilePoker;
  client = pkgs.client;
  docker = docker-container;
  shell  = haskellPackages.agilePoker.env // pkgs.client;
}
