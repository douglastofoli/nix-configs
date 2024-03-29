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
    nix-emacs.url = "github:douglastofoli/nix-emacs";

    # Elixir LSP
    lexical-lsp.url = "github:lexical-lsp/lexical";
    next-ls.url = "github:elixir-tools/next-ls";
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
        # overlays = with inputs; [
        #   helix.overlays.default
        # ];
        config = {
          allowUnfree = true;
          permittedInsecurePackages = [
            "electron-13.6.9"
            "electron-25.9.0"
          ];
        };
      };

      vars = {
        user = "douglas";
        terminal = "${pkgs.wezterm}/bin/wezterm";
        editor = "${pkgs.helix}/bin/hx";
        browser = "${pkgs.firefox}/bin/firefox";
        timezone = "America/Sao_Paulo";
        stateVersion = "23.11";
      };

      user = "douglas";
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
              ${user} = {
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
