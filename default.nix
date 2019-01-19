let
  config = {
    packageOverrides = pkgs: rec {
      client =
        import ./client.nix {};

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
            cp -r ${client}/public ./public
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
in
  with pkgs;
  {
    server = haskellPackages.agilePoker;
    client = client;
    docker = docker-container;
    shell  = haskellPackages.agilePoker.env // client; # @TODO: add elm
  }
