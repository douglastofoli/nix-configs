{
  description = "A basic flake to run an Elixir Project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        inherit (pkgs.lib) optional optionals;
        pkgs = import nixpkgs { inherit system; };
        
        beamPkg = pkgs.beam.packagesWith pkgs.erlangR25;
        elixir = beamPkg.elixir.override {
          version = "1.14.3";
          sha256 = "sha256-8rkuyAQAZdaKFXnSMaIPwbgoHnPs+nJ+mdbqcqYNeE4=";
        };

        nodejs = with pkgs;
          if builtins.pathExists ./apps/schedu_me_web/assets/package.json then [
            nodejs_18
          ] else
            "";

        inputs = with pkgs; [
          elixir 
          glibcLocales
          postgresql_15
          nodejs
        ] ++ optional stdenv.isLinux [
          inotify-tools
        ] ++ optional stdenv.isDarwin terminal-notifier 
        ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
        ]);
      in 
      with pkgs;
      {
        devShells.default = mkShell {
          name = "schedume";
          packages = inputs;
        }; 
      });
}
