{ config, pkgs, ... }:

{
  services.emacs = { enable = true; };

  environment.systemPackages = with pkgs; [
    fd
    ripgrep

    nixfmt
    wakatime

    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ editorconfig emojify vterm ]))
  ];
}
