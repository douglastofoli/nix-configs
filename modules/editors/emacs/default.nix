{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    fd
    ripgrep
    nixfmt

    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ editorconfig vterm ]))
  ];
}
