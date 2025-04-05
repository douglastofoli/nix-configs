{
  config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
  exec = "exec Hyprland";
in {
  options.hyprland = {
    enable = mkEnableOption {
      description = "Enables Hyprland";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.hyprland.enable {
    programs = {
      hyprland = {
        enable = true;
        withUWSM = true;
      };

      waybar.enable = true;
    };

    environment = {
      loginShellInit = ''
        if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
          ${exec}
        fi
      '';
    };
  };
}
