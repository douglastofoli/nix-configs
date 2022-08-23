# Home manager configuration for desktop

{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      # Editors
      # editorconfig-checker
      # coreutils
      # fd
      # nixfmt
      # ripgrep
  
      vscodium
      gimp
      insomnia
      obsidian
      bpytop
      obinskit
      gnome.file-roller
      pcmanfm
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
