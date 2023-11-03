{
  description = "My Personal NixOS System Flake Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-emacs.url = "github:douglastofoli/nix-emacs";

    helix.url = "github:helix-editor/helix/23.10";

    # Elixir LSP
    lexical-lsp.url = "github:lexical-lsp/lexical?ref=v0.4.0";
    next-ls.url = "github:elixir-tools/next-ls?ref=v0.14.1";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    nix-emacs,
    ...
  }: let
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
        inherit inputs nixpkgs home-manager nix-emacs vars;
      }
    );
  };
}
