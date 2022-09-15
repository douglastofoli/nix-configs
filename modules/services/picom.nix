{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;

    package = pkgs.picom.overrideAttrs (_: {
      src = pkgs.fetchFromGitHub {
        repo = "picom";
        owner = "jonaburg";
        rev = "e3c19cd7d1108d114552267f302548c113278d45";
        sha256 = "sha256-4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
      };
    });

    # Shadown
    shadow = true;
    shadowOpacity = 0.6;
    shadowOffsets = [ (-3) (-3) ];
    shadowExclude = [
      "name = 'Notification'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'xmobar'"
      "class_g = 'trayer'"
      "_GTK_FRAME_EXTENTS@:c"
    ];

    # Fading
    fade = true;
    fadeSteps = [ 3.0e-2 3.0e-2 ];

    # Transparency
    activeOpacity = 1.0;
    inactiveOpacity = 0.8;

    settings = {
      # Shadow
      shadow-radius = 8;

      # Animations
      transition-length = 180;
      transition-pow-x = 0.1;
      transition-pow-y = 0.1;
      transition-pow-w = 0.1;
      transition-pow-h = 0.1;
      size-transition = true;

      corner-radius = 10.0;
      round-borders = 1;
      round-borders-exclude =
        [ "class_g = 'Cairo-clock'" "class_g = 'xmobar'" "class_g = 'trayer'" ];
      rounded-corners-exclur =
        [ "class_g = 'Cairo-clock'" "class_g = 'xmobar'" "class_g = 'trayer'" ];

      # Opacity
      frame-opacity = 0.7;
      opacity-rule = [
        "100:class_g = 'firefox'"
        "100:class_g = 'google-chrome'"
        "100:class_g = 'Thunderbird'"
      ];

      # Blur
      blur = {
        method = "kawase";
        strength = 7;
        background = false;
        background-frame = false;
        background-fixed = false;
        kern = "3x3box";
      };
    };

    menuOpacity = 0.9;

    # Backend
    backend = "glx";
    experimentalBackends = true;

    vSync = true;

    wintypes = {
      normal = {
        fade = false;
        shadow = true;
      };
      tooltip = {
        fade = true;
        shadow = true;
        opacity = 0.75;
        focus = true;
        full-shadow = false;
      };
      dock = { shadow = false; };
      dnd = { shadow = true; };
      popup_menu = { opacity = config.services.picom.menuOpacity; };
      dropdown_menu = { opacity = config.services.picom.menuOpacity; };
    };
  };
}
