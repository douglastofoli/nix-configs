# Profiles that can be used when building Nix

{ lib, inputs, nixpkgs, home-manager, nixgl, user, system, pkgs, ... }:

{
  pacman =
    home-manager.lib.homeManagerConfiguration { # Currently only host that can be built
      inherit pkgs;

      modules = [ ./pacman.nix ];
      extraSpecialArgs = { inherit inputs nixgl user; };
    };
}
