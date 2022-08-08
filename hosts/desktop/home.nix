# Home manager configuration for desktop

{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      discord
      feh
      gimp
      gnome.file-roller
      pcmanfm
      insomnia
      obsidian
    ];
  };

  services.blueman-applet.enable = true;
}
