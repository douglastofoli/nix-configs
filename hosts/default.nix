# These are the different profiles that can be used when building NixOS

{ inputs, home-manager, nur, system, pkgs, lib, user, location, protocol, ... }:

{
  desktop = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs user location protocol; };
    modules = [
      nur.nixosModules.nur
      ./desktop
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = { inherit user protocol; };
        home-manager.users.${user} = {
          imports = [ (import ./home.nix) ] ++ [ (import ./desktop/home.nix) ];
        };
      }
    ];
  };

  solfacil = lib.nixosSystem { # Solfácil Laptop profile
    inherit system;
    specialArgs = { inherit inputs user location protocol; };
    modules = [
      nur.nixosModules.nur
      ./solfacil
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = { inherit user protocol; };
        home-manager.users.${user} = {
          imports = [ (import ./home.nix) ] ++ [ (import ./laptop/home.nix) ];
        };
      }
    ];
  };
}
