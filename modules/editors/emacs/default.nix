{ config, pkgs, ... }:

{
  services.emacs = {
    enable = true;

  };
  environment.systemPackages = with pkgs; [
    shfmt
    shellcheck

    html-tidy
    nodePackages.stylelint
    nodePackages.js-beautify

    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ editorconfig vterm ]))
  ];
}
