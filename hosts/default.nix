# These are the different profiles that can be used when building NixOS

{ lib, inputs, nixpkgs, nixpkgs-pinned, agenix, home-manager, nur, user
, location, ... }:

let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };

  pkgs-pinned = import nixpkgs-pinned {
    inherit system;
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;

  insync-v3 =
    pkgs-pinned.libsForQt5.callPackage ../modules/programs/insync-v3.nix {
      alsaLib = pkgs.alsaLib;
    };
in {
  desktop = lib.nixosSystem { # Desktop profile
    inherit system;
    specialArgs = {
      inherit inputs user location;
      host = { # System specific configuration
        hostName = "desktop";
        gitSigningKey = "C81F647F6A1A0F62";
      };
    };
    modules = [
      agenix.nixosModules.default
      nur.nixosModules.nur
      ./desktop
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.extraSpecialArgs = {
          inherit user insync-v3;
          host = { # User specific configuration
            hostName = "desktop";
            alacrittyFontSize = 11;
          };
        };
        home-manager.users.${user} = {
          imports = [ ./home.nix ./desktop/home.nix ];
        };
      }
    ];
  };
}
