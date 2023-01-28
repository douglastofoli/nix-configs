# Profiles that can be used when building Nix

{ lib, inputs, nixpkgs, home-manager, nixgl, user, ... }:

let
  system = "x86_64-linux";
  pkgs = nixpkgs.legacyPackages.${system};
in {
  pacman = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit inputs nixgl user;
    };
    modules = [
      ./pacman.nix
      {
        home = {
          username = "${user}";
          homeDirectory = "/home/${user}";
          packages = [ pkgs.home-manager ];
          stateVersion = "22.11";
        };
      }
    ];
  };
}
