{
  config,
  lib,
  pkgs,
  vars,
  ...
}: let
  colors = import ../themes/colors.nix;
  c = colors.scheme.catppuccin-macchiato;
in {
  config = lib.mkIf config.hyprland.enable {
    home-manager.users.${vars.user} = {
      home.packages = [pkgs.libnotify];
      
      services.swaync.enable = true;
    };
  };
}
