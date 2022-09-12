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
      helvum
      insomnia
      minecraft
      neofetch
      obinskit
      obsidian
      obs-studio
      pcmanfm
      qbittorrent
    ];
  };

  programs.git.signing.key = "A78F1B85248F4095";

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
