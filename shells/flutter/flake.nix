{
  description = "Flutter 3.0.4";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true;
          };
        };
        buildToolsVersion = "30.0.3";
        androidComposition = pkgs.androidenv.composeAndroidPackages {
          buildToolsVersions = [ buildToolsVersion "28.0.3" ];
          platformVersions = [ "31" "28" ];
          abiVersions = [ "armeabi-v7a" "arm64-v8a" ];
        };
        androidSdk = androidComposition.androidsdk;

        dartVersion = "3.1.0";
        dartSourceBase = "https://storage.googleapis.com/dart-archive/channels";
        dart = pkgs.dart.override {
          version = dartVersion;
          sources = {
            "${dartVersion}-x86_64-linux" = pkgs.fetchurl {
              url =
                "${dartSourceBase}/stable/release/${dartVersion}/sdk/dartsdk-linux-x64-release.zip";
              sha256 = "sha256-fOXBVgsdjqXuDm1E8cPnuATdxTd7ONcv5NQwCabGfoQ=";
            };
          };
        };
      in {
        devShell = with pkgs;
          mkShell rec {
            ANDROID_SDK_ROOT = "${androidSdk}/libexec/android-sdk";
            nativeBuildInputs = [ cmake ninja ];
            buildInputs =
              [ androidSdk cacert clang dart flutter gtk3 jdk11 pkgconfig ];
          };
      });
}

