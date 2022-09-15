# Home manager configuration for desktop

{ pkgs, ... }:

{
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
      thunderbird
    ];
  };

  programs.alacritty.settings.font.size = 12;
  programs.git.signing.key = "A78F1B85248F4095";

  services.blueman-applet.enable = true;
}
