# These are the different profiles that can be used when building NixOS

{ lib, inputs, nixpkgs, home-manager, nur, user, location, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;
in {
  desktop = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs user location; };
    modules = [
      nur.nixosModules.nur
      ./desktop
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = { inherit user; };
        home-manager.users.${user} = {
          imports = [ (import ./home.nix) ] ++ [ (import ./desktop/home.nix) ];
        };
      }
    ];
  };
}
