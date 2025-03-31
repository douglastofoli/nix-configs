{
  description = "A basic flake to run an Elixir Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        inherit (pkgs.lib) optional optionals;
        pkgs = import nixpkgs { inherit system; };

        beamPkg = pkgs.beam.packagesWith pkgs.erlang_27;
        elixir = beamPkg.elixir.override {
          version = "1.18.3";
          sha256 = "sha256-jH+1+IBWHSTyqakGClkP1Q4O2FWbHx7kd7zn6YGCog0=";
        };

        exercism-pkg = pkgs.exercism;

        inputs =
          with pkgs;
          [
            elixir
            glibcLocales
          ]
          ++ optional stdenv.isLinux inotify-tools
          ++ optional stdenv.isDarwin terminal-notifier
          ++ optionals stdenv.isDarwin (
            with darwin.apple_sdk.frameworks;
            [
              CoreFoundation
              CoreServices
            ]
          )
          ++ [ exercism-pkg ];
      in
      {
        devShells.default = pkgs.mkShell {
          name = "project";
          packages = inputs;
        };
      }
    );
}
