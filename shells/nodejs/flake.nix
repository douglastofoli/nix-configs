{
  description = "A basic flake to run an Nodejs Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        inputs = with pkgs; [ nodejs_18 ];
      in with pkgs; {
        devShells.default = mkShell {
          name = "nodejs";
          packages = inputs;
        };
      });
}
