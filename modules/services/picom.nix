{ config, lib, pkgs, ... }:

{
  config = lib.mkIf (config.xsession.enable) {
    services.picom = {
      enable = true;
      package = pkgs.picom.overrideAttrs (o: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "jonaburg";
          rev = "e3c19cd7d1108d114552267f302548c113278d45";
          sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y";
        };
      });

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
