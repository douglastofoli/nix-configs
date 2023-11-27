{
  config,
  lib,
  vars,
  ...
}: {
  config = lib.mkIf config.xmonad.enable {
    home-manager.users.${vars.user} = {
      services.picom = {
        enable = true;

        settings = {
          # Corners
          corner-radius = 8;
          rounded-corners-exclude = ["window_type = 'dock'" "window_type = 'desktop'"];

          # Shadows
          shadow = false;
          shadow-radius = 7;
          shadow-offset-x = -7;
          shadow-offset-y = -7;
          shadow-exclude = [
            "name = 'Notification'"
            "class_g = 'Conky'"
            "class_g ?= 'Notify-osd'"
            "class_g = 'Cairo-clock'"
            "_GTK_FRAME_EXTENTS@:c"
          ];

          # Fading
          fading = true;
          fade-in-step = 0.03;
          fade-out-step = 0.03;
          fade-exclude = [
            "class_g = 'slop'" # maim
          ];

          # Transparancy / Opacity
          active-opacity = 1.0;
          inactive-opacity = 0.8;
          frame-opacity = 0.7;
          popup_menu = {opacity = 0.8;};
          dropdown_menu = {opacity = 0.8;};
          inactive-opacity-override = false;

          focus-exclude = [
            "class_g = 'Cairo-clock'"
            "class_g = 'slop'" # maim
          ];

          opacity-rule = [
            "80:class_g = 'xmobar'"
          ];

          # Background-Blurring
          blur-kern = "3x3box";
          blur-background-exclude = [
            "window_type = 'dock'"
            "window_type = 'desktop'"
            "_GTK_FRAME_EXTENTS@:c"
          ];

          # General settings
          backend = "glx";
          dithered-present = false;
          vsync = true;
          mark-wmwin-focused = true;
          mark-ovredir-focused = true;
          detect-rounded-corners = true;
          detect-client-opacity = true;
          detect-transient = true;
          use-damage = true;
          log-level = "warn";

          wintypes = {
            tooltip = {
              fade = true;
              shadow = true;
              opacity = 0.75;
              focus = true;
              full-shadow = false;
            };
            dock = {
              shadow = false;
              clip-shadow-above = true;
            };
            dnd = {shadow = false;};
            popup_menu = {opacity = 0.8;};
            dropdown_menu = {opacity = 0.8;};
          };
        };
      };
    };
  };
}
