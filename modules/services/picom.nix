{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (config.xsession.enable) {
    services.picom = {
      enable = true;
      
      backend = "glx";
      vSync = true;
      activeOpacity = "0.93"; # Node transparency
      inactiveOpacity = "0.93";
      menuOpacity = "0.93";
      shadow = false; # Shadows
      shadowOpacity = "0.75";
      fade = true; # Fade
      fadeDelta = 10;

      opacityRule = [ # Opacity rules if transparency is prefered
        "100:name = 'Picture in picture'"
        "100:name = 'Picture-in-Picture'"
        "85:class_i ?= 'rofi'"
        "80:class_i *= 'discord'"
        "80:class_i *= 'emacs'"
        "80:class_i *= 'Alacritty'"
        "100:fullscreen"
      ];
    };
  };
}
