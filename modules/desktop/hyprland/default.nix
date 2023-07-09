{ config, pkgs, system, hyprland, ... }:

let exec = "exec Hyprland";
in {
  programs = {
    hyprland = {
      enable = true;

      xwayland = {
        enable = true;
        hidpi = false;
      };

      nvidiaPatches = false;
    };

    waybar.enable = true;
  };

  environment = {
    loginShellInit = ''
      if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        ${exec}
      fi
    '';

    systemPackages = with pkgs; [
      grim
      mpvpaper
      playerctl
      slurp
      swappy
      swaybg
      swayidle
      swaylock-effects
      wl-clipboard
      wlr-randr
      wlogout
    ];
  };

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  nixpkgs.overlays = [
    (final: prev: { waybar = hyprland.packages.${system}.waybar-hyprland; })
  ];
}
