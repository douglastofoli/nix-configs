# Home manager configuration for desktop

{ pkgs, ... }:

{
  # imports = [ (import ../../modules/editors/vscode.nix) ];

  home = {
    packages = with pkgs; [
      asdf-vm
      bat
      bpytop
      exa
      gimp
      gnome.file-roller
      insomnia
      minecraft
      neofetch
      obinskit
      obsidian
      pcmanfm
    ];
  };

  programs.alacritty.settings.font.size = 12;

  services.blueman-applet.enable = true;
}
