{ config, pkgs, ... }:

{
  services.emacs = {
    enable = true;

  };
  environment.systemPackages = with pkgs;
    [

      ((emacsPackagesFor emacs).emacsWithPackages
        (epkgs: with epkgs; [ editorconfig vterm ]))
    ];
}
