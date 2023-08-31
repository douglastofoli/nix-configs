{ config, pkgs, ... }:

{
  services.picom = {
    enable = true;

    #Shadows
    shadow = true;
    shadowOffsets = [ (-7) (-7) ];
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'slop'"
      "_GTK_FRAME_EXTENTS@:c"
    ];

    # Fading
    fade = true;
    fadeSteps = [ 3.0e-2 3.0e-2 ];

    # Transparancy / Opacity
    inactiveOpacity = 0.8;

    # General settings
    backend = "glx";
    vSync = true;

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
      dnd = { shadow = false; };
      popup_menu = { opacity = 0.8; };
      dropdown_menu = { opacity = 0.8; };
    };

    settings = {
      # Shadows
      shadow-radius = 7;

      # Transparancy / Opacity
      frame-opacity = 0.7;
      inactive-opacity-override = false;
      focus-exclude = [ "class_g = 'Cairo-clock'" ];

      # Corners
      corner-radius = 8;
      rounded-corners-exclude =
        [ "window_type = 'dock'" "window_type = 'desktop'" ];

      # Background-Blurring
      blur-kern = "3x3box";
      blur-background-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "_GTK_FRAME_EXTENTS@:c"
      ];

      # General settings
      dithered-present = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      detect-transient = true;
      use-damage = true;
      log-level = "warn";
    };
  };
}
