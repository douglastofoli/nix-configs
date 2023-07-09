{ pkgs, ... }:

{
  programs.rofi = { enable = true; };

  #xdg.configFile."rofi".source = ../../dotfiles/rofi;
}
