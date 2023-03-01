{ config, lib, pkgs, user, ... }:

let inherit (config.lib.formats.rasi) mkLiteral;
in {
  programs.rofi = { enable = true; };

  xdg.configFile."rofi/bin".source = ../../dotfiles/rofi/bin;
  xdg.configFile."rofi/config".source = ../../dotfiles/rofi/config;
}
