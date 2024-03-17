{
  description = "A basic flake to run a Clojure Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      inherit (pkgs.lib) optionals;
      pkgs = import nixpkgs {inherit system;};

      inputs = with pkgs;
        [clojure leiningen]
        ++ optionals stdenv.isDarwin
        (with darwin.apple_sdk.frameworks; [CoreFoundation CoreServices]);
    in
      with pkgs; {
        devShells.default = mkShell {
          name = "clojure";
          packages = inputs;
        };
      });
}
