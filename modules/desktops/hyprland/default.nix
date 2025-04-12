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
      description = "Enables Hyprland with updated features";
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

    environment.variables = {
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland,x11,*";
      MOZ_ENABLE_WAYLAND = "1";
    };
  };
}
