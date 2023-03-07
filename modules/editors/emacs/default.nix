{ config, pkgs, ... }:

{
  services.emacs = { enable = true; };

  environment.systemPackages = with pkgs; [
    ripgrep
    fd

    texlive.combined.scheme-medium
    nixfmt
    graphviz
    html-tidy
    wakatime
    shellcheck
    shfmt
    nodePackages.stylelint
    nodePackages.js-beautify
    pandoc

    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ editorconfig vterm yasnippet ]))
  ];
}
