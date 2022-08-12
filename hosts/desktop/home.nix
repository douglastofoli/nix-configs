# Home manager configuration for desktop

{ pkgs, ... }:

{
  home = { packages = with pkgs; [ discord gimp insomnia obsidian ]; };

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
