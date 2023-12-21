{
  pkgs,
  vars,
  ...
}: {
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/${vars.user}";

  programs.home-manager.enable = true;

  services.blueman-applet.enable = true;

  home.packages = with pkgs; [
    bat
    obsidian
  ];
}
