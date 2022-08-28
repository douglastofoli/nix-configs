# Home manager configuration for desktop

{ pkgs, ... }:

{
  imports = [ (import ../../modules/editors/vscode.nix) ];

  home = {
    packages = with pkgs; [
      # Editors
      coreutils
      fd
      nixfmt
      ripgrep

      gimp
      insomnia
      obsidian
      bpytop
      obinskit
      gnome.file-roller
      pcmanfm
      minecraft
      exa
      bat
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
