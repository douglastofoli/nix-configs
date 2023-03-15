{
  description = "A basic flake to run an Elixir Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell-flake.url = "github:numtide/devshell";
  };

  outputs = { self, nixpkgs, flake-utils, devshell-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        javascriptDeps = with pkgs;
          if builtins.pathExists ./assets/package.json then [
            nodejs
            yarn
          ] else
            [ ];

        beamPkg = pkgs.beam.packagesWith pkgs.erlangR25;
        elixir = beamPkg.elixir.override {
          version = "1.14.3";
          sha256 = "sha256-8rkuyAQAZdaKFXnSMaIPwbgoHnPs+nJ+mdbqcqYNeE4=";
        };
      in with pkgs; rec {
        devShells.default =
          callPackage ./shell.nix { inherit javascriptDeps elixir; };
      });
}
