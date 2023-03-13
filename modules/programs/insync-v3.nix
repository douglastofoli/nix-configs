{ stdenv, lib, fetchurl, makeWrapper, dpkg, glibc, glib, libxcb, libGL, nss
, libthai, wayland, alsaLib, qtvirtualkeyboard, qtwebchannel, qtwebsockets
, qtlocation, qtwebengine, autoPatchelfHook, wrapQtAppsHook }:

stdenv.mkDerivation rec {

  pname = "insync";
  version = "3.8.4.50481";
  src = fetchurl {
    url =
      "https://cdn.insynchq.com/builds/linux/${pname}_${version}-focal_amd64.deb";
    sha256 = "sha256-VO07pD5wmhnX9QCrFS8vr699ofgToM5/fZBnri9/L3M=";
  };

  postPatch = ''
    substituteInPlace usr/bin/insync --replace /usr/lib/insync $out/usr/lib/insync
  '';

  buildInputs = [
    alsaLib
    libGL
    libthai
    libxcb
    nss
    qtlocation
    qtvirtualkeyboard
    qtwebchannel
    qtwebengine
    qtwebsockets
    wayland
  ];

  nativeBuildInputs = [ autoPatchelfHook dpkg makeWrapper wrapQtAppsHook ];

  unpackPhase = ''
    dpkg-deb --fsys-tarfile $src | tar -x --no-same-permissions --no-same-owner
  '';

  installPhase = ''
    mkdir -p $out/bin $out/lib $out/share
    cp -R usr/* $out/
    rm $out/lib/insync/libGLX.so.0
    rm $out/lib/insync/libdrm*.so*
    rm $out/lib/insync/libxkbcommon*.so*
    rm $out/lib/insync/libQt5*
    sed -i 's|/usr/lib/insync|/lib/insync|' "$out/bin/insync"
    wrapQtApp "$out/lib/insync/insync"
  '';

  dontConfigure = true;
  dontBuild = true;

  meta = with lib; {
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
    maintainers = with maintainers; [ benley ];
    homepage = "https://www.insynchq.com";
    description = "Google Drive sync and backup with multiple account support";
    longDescription = ''
      Insync is a commercial application that syncs your Drive files to your
      computer.  It has more advanced features than Google's official client
      such as multiple account support, Google Doc conversion, symlink support,
      and built in sharing.

      There is a 15-day free trial, and it is a paid application after that.
    '';
  };
}
