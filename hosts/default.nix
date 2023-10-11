# These are the different profiles that can be used when building NixOS

{ lib, inputs, nixpkgs, home-manager, nur, nix-emacs, vars, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;
in {
  desktop = lib.nixosSystem { # Desktop profile
    inherit system;
    specialArgs = {
      inherit inputs system nix-emacs vars;
      host = { # System specific configuration
        hostName = "desktop";
        gitSigningKey = "A30D5C3DE5FCB642";
        defaultBrowser = "firefox";
      };
    };
    modules = [
      nur.nixosModules.nur
      ./desktop
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
      }
    ];
  };
}
