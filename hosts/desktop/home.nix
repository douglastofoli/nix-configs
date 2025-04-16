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
    kitty
    anydesk

    bat
    eza
    zoxide
    obsidian
    ffmpeg

    zed-editor
    zen-browser
  ];
}
