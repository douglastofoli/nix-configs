{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    
    # Shadown
    shadow = true;
    shadowOffsets = [
      (-7)
      (-7)
    ];
    shadowOpacity = 0.0;
    shadowExclude = [
      "name = 'Notification'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
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
    inactiveOpacity = 0.95;
    opacityRules = [
      "100:class_g = 'firefox'"
      "80:class_g = 'i3bar' && !_NET_WM_STATE@:32a"
      "50:class_g = 'i3-frame' && !_NET_WM_STATE@:32a"
      "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      "100:fullscreen"
    ];

    # Background-Blurring
    settings = {
      blur = {
        method = "dual_kawase";
        strength = 8;
        background = false;
        background-frame = false;
        background-fixed = false;
        kern = "3x3box";
        blur-background-exclude = [
          "class_g = 'TelegramDesktop'"
          "window_type = 'desktop'"
          "_GTK_FRAME_EXTENTS@:c"
        ];
      };
    };

    # Backend
    backend = "glx";
    experimentalBackends = true;

    vSync = true;

    wintypes = {
      tooltip = { fade = true; shadow = true; opacity = 0.75; focus = true; full-shadow = false; };
      popup_menu = { opacity = config.services.picom.menuOpacity; };
      dropdown_menu = { opacity = config.services.picom.menuOpacity; };
    };
  };
}
