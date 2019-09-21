{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, containers, cookie, gitrev, http-types, mtl, random, servant
, servant-server, servant-websockets, stdenv, text, time, wai
, wai-extra, wai-middleware-static, warp, websockets
}:
mkDerivation {
  pname = "planning-game";
  version = "0.2.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers cookie
    gitrev http-types mtl random servant servant-server
    servant-websockets text time wai wai-extra wai-middleware-static
    warp websockets
  ];
  license = stdenv.lib.licenses.agpl3;
}
