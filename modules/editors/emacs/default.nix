{ config, pkgs, ... }:

{
  services.emacs = { enable = true; };

  environment.systemPackages = with pkgs; [
    # base
    fd
    ripgrep

    # aspell
    aspell
    aspellDicts.en
    aspellDicts.pt_BR

    # javascript
    html-tidy
    nodePackages.stylelint
    nodePackages.js-beautify

    # markdown
    multimarkdown

    # nix
    nixfmt

    # org
    graphviz

    # sh
    shellcheck
    shfmt

    # wakatime-mode
    wakatime

    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ editorconfig vterm ]))
  ];
}
