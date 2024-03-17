{
  description = "Dev envinronment for Java";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell-flake.url = "github:numtide/devshell";
  };

  outputs = { self, nixpkgs, flake-utils, devshell-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };

      in with pkgs; rec { devShells.default = callPackage ./shell.nix { }; });
}
