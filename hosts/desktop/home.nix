{
  pkgs,
  vars,
  ...
}: {
  home.stateVersion = "${vars.stateVersion}";
  home.homeDirectory = "/home/${vars.user}";

  programs.home-manager.enable = true;

  services.blueman-applet.enable = true;

  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en_US
    hunspellDicts.pt_BR

    #nvim-pkg
    neovim-unwrapped
    sqlite

    cura-appimage

    prismlauncher
    jdk8

    anydesk

    bat
    eza
    zoxide
    ffmpeg
    codex

    obsidian

    slack
  ];
}
