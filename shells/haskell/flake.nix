{
  description = "A basic flake to run a Haskell Project";

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
      inherit (pkgs.lib) optional optionals;
      pkgs = import nixpkgs {inherit system;};

      inputs = with pkgs; [ghc ghcid];
    in
      with pkgs; {
        devShells.default = mkShell {
          name = "haskell";
          packages = inputs;
        };
      });
}
