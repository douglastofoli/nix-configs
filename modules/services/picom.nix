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
  };
}
