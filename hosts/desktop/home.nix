# Home manager configuration for desktop

{ pkgs, ... }:

{
  # imports = [ (import ../../modules/editors/vscode.nix) ];

  home = {
    packages = with pkgs; [
      asdf-vm
      bat
      bpytop
      etcher
      exa
      feh
      gimp
      gnome.file-roller
      gnome.seahorse
      insomnia
      minecraft
      neofetch
      obinskit
      obsidian
      pcmanfm
      qbittorrent
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
