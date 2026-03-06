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
    nvim.url = "github:douglastofoli/nvim";

    # Elixir LSP
    expert-lsp.url = "github:elixir-lang/expert";
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
          #nvim.overlays.default
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
        browser = "${pkgs.firefox}/bin/firefox";
        timezone = "America/Sao_Paulo";
        stateVersion = "25.11";
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
                inherit (inputs) helix expert-lsp;
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
