{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR"; # NUR packages
    };

    nixgl = { # OpenGL
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-pinned = {
      url = "github:nixos/nixpkgs/b49473e6679c733f917254b786bfac42339875eb";
    };
  };

  outputs =
    inputs@{ self, nixpkgs, nixpkgs-pinned, home-manager, nur, nixgl, ... }:
    let
      system = "x86_64-linux"; # System architecture

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      lib = nixpkgs.lib;

      user = "douglas";
      location = "$HOME/.setup";
      protocol = "X";
    in {
      nixosConfigurations = ( # NixOS configurations
        import ./hosts {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs-pinned home-manager nur system pkgs user
            location protocol;
        });

      homeConfigurations = ( # Non-NixOS configurations
        import ./nix {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs home-manager nixgl user system pkgs;
        });
    };
}
