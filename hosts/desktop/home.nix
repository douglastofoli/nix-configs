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

    kitty
    anydesk

    bat
    eza
    zoxide
    ffmpeg

    logseq

    zed-editor
    zen-browser
  ];
}
