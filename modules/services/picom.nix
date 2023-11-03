{
  config,
  lib,
  pkgs,
  vars,
  ...
}: {
  config = lib.mkIf config.xmonad.enable {
    home-manager.users.${vars.user} = {
      services.picom = {
        enable = true;
        package = pkgs.picom-jonaburg;

        settings = {
          # Animations
          transition-length = 300;
          transition-pow-x = 0.1;
          transition-pow-y = 0.1;
          transition-pow-w = 0.1;
          transition-pow-h = 0.1;
          size-transition = true;

          # Corners
          corner-radius = 8;
          rounded-corners-exclude = ["window_type = 'dock'" "window_type = 'desktop'"];
          round-borders = 2;

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
            "class_g = 'slop'"
            "class_g = 'xmobar'"
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
            "class_g = 'xmobar'" # xmobar
            "class_g = 'slop'" # maim
          ];

          opacity-rule = [
            "80:class_g     = 'xmobar'"
            "100:class_g    = 'slop'"
            "100:class_g    = 'XTerm'"
          ];

          # Background-Blurring
          blur-kern = "3x3box";
          blur-method = "kawase";
          blur-size = 7;
          blur-background = false;
          blur-background-frame = false;
          blur-background-fixed = false;
          blur-background-exclude = [
            "window_type = 'slop'"
            "_GTK_FRAME_EXTENTS@:c"
          ];

          # General settings
          backend = "glx";
          vsync = true;
          mark-wmwin-focused = true;
          mark-ovredir-focused = true;
          detect-rounded-corners = true;
          detect-client-opacity = true;
          detect-transient = true;
          detect-client-leader = true;
          use-damage = false;
          refresh-rate = 0;
          log-level = "info";

          wintypes = {
            normal = {
              fade = false;
              shadow = false;
            };
            tooltip = {
              fade = true;
              shadow = true;
              opacity = 0.75;
              focus = true;
              full-shadow = false;
            };
            dock = {
              shadow = false;
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
