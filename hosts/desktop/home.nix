{pkgs, ...}: {
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/douglas";

  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    bat
  ];
}
