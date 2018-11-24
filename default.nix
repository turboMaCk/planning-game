let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages;

  elmPackages = pkgs.elmPackages;
in
  pkgs.haskellPackages.mkDerivation {
        pname = "agile-pocker";
        version = "1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = with haskellPackages;
                                   [ text containers random bytestring
                                     aeson servant servant-server wai warp
                                     websockets servant-websockets wai-middleware-static
                                     http-types
                                     elmPackages.elm
                                   ];
        license = pkgs.stdenv.lib.licenses.bsd3;
    }
