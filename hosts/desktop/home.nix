# Home manager configuration for desktop

{ pkgs, ... }:

{
  imports = [ (import ../../modules/editors/vscode.nix) ];

  home = {
    sessionVariables = {
      SSH_AUTH_SOCK = "/run/user/1000/keyring/ssh";
    };

    packages = with pkgs; [
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
