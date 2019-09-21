let
  config = {
    packageOverrides = pkgs: rec {
      elm =
        import ./nix/client.nix {};

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
              haskellPackagesNew.callPackage ./nix/server.nix {};

            # statically liked version (used for docker)
            planningGame-mini =
              pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callPackage ./nix/server.nix {});
          };
        };
    };
  };

  pkgs =
    # nixpkgs 19.03 as of 19/08
    import ((import <nixpkgs> {}).fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "67135fbcc5d5d28390c127ef519b09a362ef2466";
        sha256 = "00591607zmn1hfjs4959ksh164b0gjqwkvbrc4anx6da8xmhfcc2";
    }) { inherit config; };
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
