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
    bat
    ffmpeg
    spotdl
    obsidian
  ];
}
