# Home manager configuration for desktop

{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      etcher
      gimp
      discord
      obinskit
      obsidian
      qbittorrent
    ];
  };

  programs.alacritty.settings.font.size = 12;
  programs.git.signing.key = "A78F1B85248F4095";

  services.blueman-applet.enable = true;
}
