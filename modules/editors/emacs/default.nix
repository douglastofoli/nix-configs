{ config, pkgs, ... }:

{
  services.emacs = { enable = true; };

  environment.systemPackages = with pkgs; [
    ripgrep
    fd
    texlive.combined.scheme-medium
    nixfmt
    wakatime
    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
  ];
}
