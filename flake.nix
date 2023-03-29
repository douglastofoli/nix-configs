{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = { url = "github:nix-community/NUR"; };

    nixgl = { # OpenGL
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-pinned = {
      #url = "github:nixos/nixpkgs/b49473e6679c733f917254b786bfac42339875eb";
      url = "github:nixos/nixpkgs/03c01b729993503ec305de2c365cae6b1eaf1891";
    };

    agenix.url = "github:ryantm/agenix";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, nur, nixgl, nixpkgs-pinned
    , agenix, ... }:
    let
      user = "douglas";
      location = "$HOME/.setup";
    in {
      nixosConfigurations = ( # NixOS configurations
        import ./hosts {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs nixpkgs-pinned agenix home-manager nur user
            location;
        });

      homeConfigurations = ( # Non-NixOS configurations
        import ./nix {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs home-manager nixgl user;
        });
    };
}
