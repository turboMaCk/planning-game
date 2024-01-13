let
  nixpkgs =
    # updated 28/08/20
    builtins.fetchTarball {
      url = "https://github.com/NisOS/nixpkgs-channels/archive/c59ea8b.tar.gz";
      sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
    };

  config = {
    packageOverrides = pkgs: rec {
      elm =
        import ./nix/client.nix { inherit nixpkgs; };

      # Build docker container
      # Using staticaly liked version
      # This makes a huge difference in container size
      #    - Dynamically linked build container: 664M
      #    - Statically linked build container: 12M
      docker-container =
        pkgs.dockerTools.buildImage {
          name = "planning-game";
          tag = "latest";
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
              haskellPackagesNew.callPackage ./nix/server.nix {};

            # statically liked version (used for docker)
            planningGame-mini =
              pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./nix/server.nix {});
          };
        };
    };
  };

  pkgs = import nixpkgs { inherit config; };
in
  with pkgs;
  rec {
    server = haskellPackages.planningGame;
    server-mini = haskellPackages.planningGame-mini;
    client = elm;
    docker = docker-container;
    shell  = mkShell {
      inputsFrom = [ server.env client ];
      buildInputs = [ haskellPackages.ghcid elmPackages.elm haskellPackages.hlint ];
    };
  }
