{ nix-emacs, system, pkgs, ... }:

let
  emacs = nix-emacs.packages.${system}.default.override {
    config = { package = pkgs.emacs29; };
  };
in { environment.systemPackages = [ emacs ]; }
