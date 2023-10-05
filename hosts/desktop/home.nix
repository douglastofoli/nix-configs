{ config, lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      hugo
      jetbrains.datagrip
      gimp
      discord
      tdesktop
      qbittorrent
      spotify
      slack
      insomnia
      obsidian
      pcmanfm
      gnome.file-roller
      obinskit
      vscode

      brave
      logseq
      nixfmt
      fractal
      fluffychat
      dbeaver

      # emacs/nvim
      fd
      ripgrep
    ];
  };

  services = {
    blueman-applet.enable = true;
    network-manager-applet.enable = false;
  };
}
