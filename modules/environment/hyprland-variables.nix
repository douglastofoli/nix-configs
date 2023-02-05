{ config, pkgs, ... }:

{
  home = {
    sessionVariables = {
      EDITOR = "emacs";
      BROWSER = "firefox";
      TERMINAL = "alacritty";
      QT_QPA_PLATFORMTHEME = "gtk3";
      QT_SCALE_FACTOR = "1";
      MOZ_ENABLE_WAYLAND = "1";
      SDL_VIDEODRIVER = "wayland";

      WLR_RENDERER = "vulkan";

      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
    };
  };
}
