{ config, lib, pkgs, vars, ... }:

{
  home-manager.users.${vars.user} =
    lib.mkIf config.services.xserver.enable { programs.rofi.enable = true; };

  #xdg.configFile."rofi".source = ../../dotfiles/rofi;
}
