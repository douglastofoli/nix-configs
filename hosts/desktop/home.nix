{
  pkgs,
  vars,
  ...
}: let
  neovim = pkgs.callPackage ../../modules/editors/nvim {inherit pkgs;};
in {
  home.stateVersion = "${vars.stateVersion}";
  home.homeDirectory = "/home/${vars.user}";

  programs.home-manager.enable = true;

  services.blueman-applet.enable = true;

  home.packages = with pkgs; [
    neovim

    bat
    eza
    zoxide

    obsidian

    ffmpeg
    spotdl
  ];
}
