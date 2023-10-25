{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "github:nix-community/NUR";

    nixgl = { # OpenGL
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-emacs.url = "github:douglastofoli/nix-emacs";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, nur, nixgl, nix-emacs, ... }:
    let
      vars = {
        user = "douglas";
        location = "$HOME/.setup";
        terminal = "wezterm";
        editor = "emacs";
        browser = "firefox";
      };
    in {
      nixosConfigurations = ( # NixOS configurations
        import ./hosts {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs home-manager nur nix-emacs vars;
        });

      homeConfigurations = ( # Non-NixOS configurations
        import ./nix {
          inherit (nixpkgs) lib;
          inherit inputs nixpkgs home-manager nixgl vars;
        });
    };
}
