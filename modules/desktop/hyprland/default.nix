{ config, pkgs, hyprland, ... }:

let exec = "exec Hyprland";
in {
  imports = [ (import ../../programs/waybar.nix) ];

  programs.hyprland.enable = true;

  environment = {
    # loginShellInit = ''
    #   if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
    #     ${exec}
    #   fi
    # '';

    variables = {
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "Hyprland";

      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_QPA_PLATFORMTHEME = "qt5ct";

      SDL_VIDEODRIVER = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      CLUTTER_BACKEND = "wayland";
      GDK_BACKEND = "wayland";
    };

    systemPackages = with pkgs; [
      grim
      mpvpaper
      slurp
      swappy
      swaybg
      swaylock-effects
      wofi
      wofi-emoji
      wlogout
      wl-clipboard
      wlr-randr
    ];
  };

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  xdg.portal.wlr.enable = true;
}
