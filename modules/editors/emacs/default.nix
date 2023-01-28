{ config, pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    ripgrep
    fd
    nixfmt

    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
  ];
}
