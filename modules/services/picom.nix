{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    
    # Shadown
    shadow = true;
    shadowOffsets = [
      (-3)
      (-3)
    ];
    shadowOpacity = 0.6;
    shadowExclude = [
      "name = 'Notification'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'trayer'"
      "_GTK_FRAME_EXTENTS@:c"

    ];

    # Fading
    fade = true;
    fadeSteps = [ 
      0.03 
      0.03
    ];
    fadeExclude = [];

    # Transparency
    activeOpacity = 1.0;
    inactiveOpacity = 1.0;

    settings = {
      shadow-radius = 8;
    };

    menuOpacity = 0.9;

    # Backend
    backend = "glx";
    experimentalBackends = true;

    vSync = true;

    wintypes = {
      normal = { fade = false; shadow = true; };
      tooltip = { fade = true; shadow = true; opacity = 0.75; focus = true; full-shadow = false; };
      dock = { shadow = false; };
      dnd = { shadow = true; };
      popup_menu = { opacity = config.services.picom.menuOpacity; };
      dropdown_menu = { opacity = config.services.picom.menuOpacity; };
    };
  };
}
