let
  config = {
    packageOverrides = pkgs: rec {
      elm =
        import ./client.nix {};

      # Build docker container
      # Using staticaly liked version
      # This makes a huge difference in container size
      #    - Dynamically linked build container: 664M
      #    - Statically linked build container: 12M
      docker-container =
        pkgs.dockerTools.buildImage {
          name = "planning-game";
          extraCommands = ''
            ln -s ${haskellPackages.planningGame-mini}/bin/planning-game ./planning-game
            cp -r ${elm}/public ./public
          '';
          config.Cmd = [ "${haskellPackages.planningGame-mini}/bin/planning-game" "-qg" ];
        };

      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            planningGame =
              haskellPackagesNew.callPackage ./server.nix {};

            # statically liked version (used for docker)
            planningGame-mini =
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
  rec {
    server = haskellPackages.planningGame;
    server-mini = haskellPackages.planningGame-mini;
    client = elm;
    docker = docker-container;
    shell  = mkShell {
      inputsFrom = [ server.env client ];
      buildInputs = [ haskellPackages.ghcid elmPackages.elm ];
    };
  }
