{ config, pkgs, ... }:

{
  services.picom = {
    enable = true;
    package = pkgs.picom.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        repo = "picom";
        owner = "jonaburg";
        rev = "e3c19cd7d1108d114552267f302548c113278d45";
        sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
      };
    });

    backend = "glx";
    vSync = true;

    shadow = false;
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

    settings = {
      # Animations
      transition-length = 300;
      transition-pow-x = 0.5;
      transition-pow-y = 0.5;
      transition-pow-w = 0.5;
      transition-pow-h = 0.5;
      size-transition = true;

      corner-radius = 8;
      rounded-corners-exclude = [ "name = 'xmobar'" "class_g = 'trayer'" ];

      round-borders = 8;
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

      # Extras
      daemon = true;
      use-damage = false;
      resize-damage = 1;
      refresh-rate = 0;
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
      detect-rounded-corners = true;
      detect-client-opacity = false;
      detect-transient = true;
      detect-client-leader = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      unredir-if-possible = true;
      log-level = "info";
    };
  };
}
