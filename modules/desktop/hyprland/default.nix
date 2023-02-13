{ config, pkgs, hyprland, ... }:

let exec = "exec Hyprland";
in {
  imports = [ (import ../../programs/waybar.nix) ];

  programs.hyprland.enable = true;

  environment = {
    loginShellInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        ${exec}
      fi
    '';

    variables = {
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "Hyprland";
    };

    sessionVariables = {
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

      GDK_BACKEND = "wayland";
      WLR_NO_HARDWARE_CURSORS = "1";
      MOZ_ENABLE_WAYLAND = "1";
    };

    systemPackages = with pkgs; [
      grim
      mpvpaper
      slurp
      swappy
      swaybg
      swaylock
      wofi
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
