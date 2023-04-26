{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    package = pkgs.picom-jonaburg;

    shadow = true;
    shadowOffsets = [ (-3) (-3) ];
    shadowExclude = [
      "name = 'Notification'"
      "class_g ?= 'Notify-osd'"
      "class_g ?= 'Cairo-clock'"
      "class_g = 'slop'"
      "name = 'xmobar'"
      "class_g = 'trayer'"
      "_GTK_FRAME_EXTENTS@:c"
    ];

    fade = true;
    fadeSteps = [ 3.0e-2 3.0e-2 ];
    fadeExclude = [ "class_g = 'slop'" ];

    menuOpacity = 0.8;
    activeOpacity = 1.0;
    inactiveOpacity = 0.8;

    opacityRules = [
      "100:name = 'xmobar'"
      "100:class_g = 'slop'"
      "100:class_g = 'Alacritty'"
      "100:class_g = 'Emacs'"
      "100:class_g = 'firefox'"
      "100:class_g = 'Google-chrome'"
    ];

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
      dock = { shadow = false; };
      dnd = { shadow = false; };
      popup_menu = { opacity = config.services.picom.menuOpacity; };
      dropdown_menu = { opacity = config.services.picom.menuOpacity; };
    };

    vSync = true;

    settings = {
      transition-length = 200;
      transition-pow-x = 0.1;
      transition-pow-y = 0.1;
      transition-pow-w = 0.1;
      transition-pow-h = 0.1;
      size-transition = true;

      corner-radius = 8;
      rounded-corners-exclude = [ "name = 'xmobar'" "class_g = 'trayer'" ];

      round-borders = 1;
      round-borders-exclude = [ "name = 'xmobar'" "class_g = 'trayer'" ];

      shadow-radius = 7;

      frame-opacity = 0.7;

      inactive-opacity-override = false;

      focus-exclude = [
        "class_g = 'Cairo-clock'"
        "class_g = 'slop'"
        "name = 'xmobar'"
        "class_g = 'trayer'"
      ];

      blur = {
        method = "kawase";
        strength = 7;
        background = false;
        background-frame = false;
        background-fixed = false;
        kernel = "3x3box";
      };

      blur-background-exclude =
        [ "name = 'xmobar'" "class_g = 'slop'" "_GTK_FRAME_EXTENTS@:c" ];

      experimental-backends = true;
      backend = "glx";

      daemon = true;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-client-opacity = true;
      detect-rounded-corners = true;
      resize-damage = 1;
      refresh-rate = 0;
      detect-transient = true;
      detect-client-leader = true;
      use-damage = false;
      log-level = "info";
    };
  };
}
