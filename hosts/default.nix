# These are the different profiles that can be used when building NixOS
{
  inputs,
  nixpkgs,
  home-manager,
  nix-emacs,
  vars,
  ...
}: let
  system = "x86_64-linux";

  pkgs = import nixpkgs {
    inherit system;
    overlays = [inputs.helix.overlays.default];
    config.allowUnfree = true;
  };

  lib = nixpkgs.lib;
in {
  desktop = lib.nixosSystem {
    # Desktop profile
    inherit pkgs;
    # specialArgs = {
    #   inherit inputs system nix-emacs vars;
    #   host = {
    #     # System specific configuration
    #     hostName = "desktop";
    #     gitSigningKey = "A30D5C3DE5FCB642";
    #     defaultBrowser = "firefox";
    #   };
    # };
    modules = let
      desktop.custom-config = import ./desktop/custom.nix {inherit pkgs;};
    in [
      ./desktop
      ./configuration.nix

      home-manager.nixosModules.home-manager
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          sharedModules = [../modules];

          # extraSpecialArgs = {
          #   inherit (inputs) lexical-lsp next-ls helix;
          #   custom = import ./desktop/custom.nix {inherit pkgs;};
          # };

          users = let
            args = host: {
              inherit (inputs) lexical-lsp next-ls helix;
              inherit (host) custom-config;
            };
          in {
            ${vars.user} = {
              _module.args = args desktop;
              imports = [./desktop/home.nix];
            };
          };

          #users.${vars.user} = {imports = [../modules/editors/helix];};
        };
      }
    ];
  };
}
