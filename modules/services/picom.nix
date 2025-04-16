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
          backend = "glx";
          vsync = true;
          use-damage = true;
          log-level = "warn";

          corner-radius = 10;
          rounded-corners-exclude = [
            "window_type = 'dock'"
            "window_type = 'desktop'"
            "class_g = 'xmobar'"
            "_PICOM_ROUNDED = 0"
          ];

          blur = {
            method = "kawase";
            strength = 7;
            background = true;
            background-frame = true;
            background-fixed = true;
          };

          blur-background-exclude = [
            "window_type = 'dock'"
            "window_type = 'desktop'"
            "_GTK_FRAME_EXTENTS@:c"
            "class_g = 'xmobar'"
            "name = 'maim'"
          ];

          fading = true;
          fade-in-step = 0.028;
          fade-out-step = 0.028;
          fade-delta = 5;
          no-fading-openclose = false;

          active-opacity = 1.0;
          inactive-opacity = 0.90;
          frame-opacity = 0.8;
          inactive-opacity-override = false;

          opacity-rule = [
            "80:class_g = 'xmobar'"
            "90:class_g = 'Alacritty'"
            "100:class_g = 'firefox'"
            "100:class_g = 'zen'"
          ];

          wintypes = {
            tooltip = {
              fade = true;
              shadow = false;
              opacity = 0.85;
              focus = true;
              corner-radius = 5;
            };
            dock = {
              shadow = false;
              corner-radius = 0;
            };
            popup_menu = {
              opacity = 0.85;
              corner-radius = 8;
            };
            dropdown_menu = {
              opacity = 0.85;
              corner-radius = 8;
            };
          };

          shadow = false;
          shadow-radius = 12;
          shadow-offset-x = -10;
          shadow-offset-y = -10;
          shadow-opacity = 0.7;
          shadow-exclude = [
            "name = 'Notification'"
            "class_g = 'xmobar'"
            "_GTK_FRAME_EXTENTS@:c"
          ];

          focus-exclude = [
            "class_g = 'Cairo-clock'"
            "class_g = 'xmobar'"
          ];

          mark-wmwin-focused = true;
          mark-ovredir-focused = true;
          detect-rounded-corners = true;
          detect-client-opacity = true;
          detect-transient = true;
          detect-client-leader = true;
          use-ewmh-active-win = true;
        };
      };
    };
  };
}
