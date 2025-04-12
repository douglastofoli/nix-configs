{
  pkgs,
  vars,
  ...
}: let
  neovim = pkgs.callPackage ../../modules/editors/nvim {inherit pkgs;};
in {
  imports = [
    ../../modules/desktops/hyprland/home.nix # Adicione esta linha
  ];

  home.stateVersion = "${vars.stateVersion}";
  home.homeDirectory = "/home/${vars.user}";

  programs.home-manager.enable = true;

  services.blueman-applet.enable = true;

  home.packages = with pkgs; [
    kitty

    neovim
    bat
    eza
    zoxide
    obsidian
    ffmpeg

    zed-editor
    zen-browser

    # Adicione estes pacotes necessários para o Hyprland
    waybar # Barra de status
    wofi # Launcher de aplicativos
    swaybg # Para wallpaper
    swaylock # Bloqueio de tela
    grim # Captura de tela
    slurp # Seleção de área da tela
    wl-clipboard # Gerenciamento de clipboard
  ];
}
