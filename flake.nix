{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Editor
    helix.url = "github:helix-editor/helix/23.10";

    # Elixir LSP
    lexical-lsp.url = "github:lexical-lsp/lexical?ref=v0.4.1";
    next-ls.url = "github:elixir-tools/next-ls?ref=v0.15.0";
  };

  outputs = {
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (nixpkgs.lib) nixosSystem;
  in {
    nixosConfigurations = let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = with inputs; [
          helix.overlays.default
        ];
        config.allowUnfree = true;
      };

      user = "douglas";
    in {
      desktop = nixosSystem rec {
        inherit pkgs;
        modules = let
          desktop.custom-config = import ./hosts/desktop/custom.nix {inherit pkgs;};
        in [
          ./hosts/desktop
          ./hosts/configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              sharedModules = [./modules];

              users = let
                args = host: {
                  inherit (inputs) helix lexical-lsp next-lsp;
                  inherit (host) custom-config;
                };
              in {
                ${user} = {
                  _module.args = args desktop;
                  imports = [./hosts/desktop/home.nix];
                };
              };
            };
          }
        ];
      };
    };
  };
}
