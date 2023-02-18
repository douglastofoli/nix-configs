{ config, pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    # emacs dependencies
    ripgrep
    fd

    # plugins dependencies
    nixfmt
    wakatime
    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
  ];
}
