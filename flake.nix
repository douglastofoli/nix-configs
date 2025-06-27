{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Editors
    helix.url = "github:helix-editor/helix";

    # Elixir LSP
    lexical-lsp.url = "github:lexical-lsp/lexical?ref=v0.7.3";
    next-ls.url = "github:elixir-tools/next-ls?ref=v0.23.3";

    zed-editor.url = "github:nixos/nixpkgs?ref=1efd7847d1f9ee4d02d78184bf73ba09ac1767b5";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
  };

  outputs = {
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (nixpkgs.lib) nixosSystem;
  in {
    packages.x86_64-windows.helix-windows = inputs.helix.packages.x86_64-windows.default;

    homeConfigurations = let
      system = "x86_64-linux";
      pkgs = import nixpkgs rec {
        inherit system;
        overlays = with inputs; [
          helix.overlays.default
          (final: prev: {
            zed-editor = inputs.zed-editor.legacyPackages.${final.system}.zed-editor;
          })
          (final: prev: {
            zen-browser = inputs.zen-browser.packages."${system}".default;
          })
        ];
        config.allowUnfree = true;
      };

      vars = {
        user = "douglas";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        editor = "${pkgs.helix}/bin/hx";
        browser = "${pkgs.zen-browser}/bin/zen";
        timezone = "America/Sao_Paulo";
        stateVersion = "24.11";
      };

      args = host: {
        inherit (inputs) helix lexical-lsp next-ls;
        inherit (host) custom-config;
        inherit vars;
      };
    in {
      wsl = let
        wsl.custom-config = import ./hosts/wsl/custom.nix { inherit pkgs vars; };
      in inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules/default.nix
          ./hosts/wsl/home.nix
        ];
        extraSpecialArgs = args wsl;
      };
    };

    nixosConfigurations = let
      pkgs = import nixpkgs rec {
        system = "x86_64-linux";
        overlays = with inputs; [
          helix.overlays.default
          (final: prev: {
            zed-editor = inputs.zed-editor.legacyPackages.${final.system}.zed-editor;
          })
          (final: prev: {
            zen-browser = inputs.zen-browser.packages."${system}".default;
          })
        ];
        config = {
          allowUnfree = true;
          permittedInsecurePackages = [
            "electron-13.6.9"
            "electron-25.9.0"
            "electron-27.3.11"
          ];
        };
      };

      vars = {
        user = "douglas";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        editor = "${pkgs.helix}/bin/hx";
        browser = "${pkgs.zen-browser}/bin/zen";
        timezone = "America/Sao_Paulo";
        stateVersion = "24.11";
      };
    in {
      desktop = nixosSystem rec {
        inherit pkgs;
        specialArgs = {
          inherit inputs vars;
        };
        modules = let
          desktop.custom-config = import ./hosts/desktop/custom.nix {inherit pkgs vars;};
        in [
          ./hosts/desktop/configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.sharedModules = [./modules/default.nix];

            home-manager.users = let
              args = host: {
                inherit (inputs) helix lexical-lsp next-ls;
                inherit (host) custom-config;
                inherit vars;
              };
            in {
              ${vars.user} = {
                _module.args = args desktop;
                imports = [./hosts/desktop/home.nix];
              };
            };
          }
        ];
      };
    };
  };
}
