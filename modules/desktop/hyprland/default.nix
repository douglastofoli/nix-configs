{ config, pkgs, hyprland, ... }:

let exec = "exec Hyprland";
in {
  imports = [ (import ../../programs/waybar.nix) ];

  programs.hyprland = { enable = true; };

  environment = {
    loginShellInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        ${exec}
      fi
    '';
    systemPackages = with pkgs; [
      grim
      mpvpaper
      slurp
      swappy
      swaylock
      wofi
      wl-clipboard
      wlr-randr
    ];
  };

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  xdg.portal.wlr.enable = true;
}
