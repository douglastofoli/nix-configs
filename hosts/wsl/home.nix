{
  pkgs,
  vars,
  ...
}: {
  home.stateVersion = "${vars.stateVersion}";
  home.homeDirectory = "/home/${vars.user}";

  programs.home-manager.enable = true;

  # Basic packages for WSL environment
  home.packages = with pkgs; [
    helix
    git
  ];
}
