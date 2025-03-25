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
          rounded-corners-exclude = [
            "window_type = 'dock'"
            "window_type = 'desktop'"
            "class_g = 'xmonad'"
          ];

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
            "class_g = 'xmonad'"
          ];

          # Fading
          fading = true;
          fade-in-step = 0.028;
          fade-out-step = 0.028;
          fade-delta = 5;
          no-fading-openclose = false;
          fade-exclude = [
            "class_g = 'slop'"
            "class_g = 'xmonad'"
          ];

          # Transparancy / Opacity
          active-opacity = 1.0;
          inactive-opacity = 1.0;
          frame-opacity = 1.0;
          inactive-opacity-override = false;

          focus-exclude = [
            "class_g = 'Cairo-clock'"
            "class_g = 'slop'"
            "class_g = 'xmonad'"
          ];

          opacity-rule = [
            "80:class_g = 'xmobar'"
          ];

          # Background-Blurring
          blur = {
            method = "dual_kawase";
            strength = 3;
            background = false;
            background-frame = false;
            background-fixed = false;
          };

          blur-background-exclude = [
            "window_type = 'dock'"
            "window_type = 'desktop'"
            "_GTK_FRAME_EXTENTS@:c"
            "class_g = 'xmonad'"
          ];

          # General settings
          backend = "glx";
          glx-no-stencil = true;
          glx-copy-from-front = false;
          dithered-present = false;
          vsync = true;
          mark-wmwin-focused = true;
          mark-ovredir-focused = true;
          detect-rounded-corners = true;
          detect-client-opacity = true;
          detect-transient = true;
          use-damage = true;
          log-level = "warn";
          unredir-if-possible = true;
          unredir-if-possible-delay = 0;

          wintypes = {
            tooltip = {
              fade = true;
              shadow = false;
              opacity = 0.85;
              focus = true;
            };
            dock = {
              shadow = false;
              clip-shadow-above = true;
            };
            dnd = {shadow = false;};
            popup_menu = {opacity = 0.85;};
            dropdown_menu = {opacity = 0.85;};
          };
        };
      };
    };
  };
}
