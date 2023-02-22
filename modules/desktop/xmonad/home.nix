{ config, ... }:

{
  imports = [ (import ../../programs/rofi.nix) ];

  xdg.configFile."xmonad".source = ./xmonad;
  xdg.configFile."xmobar".source = ./xmobar;

  home.file.".local/bin/kernel.sh".source = ../../../dotfiles/scripts/kernel.sh;
}
