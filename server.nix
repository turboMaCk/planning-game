{ mkDerivation, aeson, base, bytestring, containers, cookie
, http-types, mtl, random, servant, servant-server
, servant-websockets, stdenv, text, wai, wai-extra
, wai-middleware-static, warp, websockets
, libiconv, client
}:
mkDerivation {
  pname = "agile-poker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers cookie http-types mtl random
    servant servant-server servant-websockets text wai wai-extra
    wai-middleware-static warp websockets
  ];
  executableSystemDepends = [ libiconv ];
  license = stdenv.lib.licenses.agpl3;
}
