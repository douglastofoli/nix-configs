{ pkgs, ... }:

{
  services = {
    xserver = {
      enable = true;
      desktopManager.gnome.enable = true;
    };
    displayManager.gdm.enable = true;
  };

  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
  ]) ++ (with pkgs.gnome; [
    cheese
    gnome-music
    gedit
    epiphany
    geary
    evince
    gnome-characters
    totem
    tali
    iagno
    hitori
    atomix
  ]);
}
