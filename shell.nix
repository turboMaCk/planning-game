let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages;

  elmPackages = pkgs.elmPackages;

  ghc =
    haskellPackages.ghcWithHoogle
    ( hs:
      with hs;
      [ text containers random bytestring
        aeson servant servant-server wai warp
        websockets servant-websockets wai-middleware-static
        http-types
      ]
    );
in
  pkgs.mkShell {
    buildInputs = with pkgs; [ ghc haskellPackages.ghcid libiconv elmPackages.elm ];
  }
