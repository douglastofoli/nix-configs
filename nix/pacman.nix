# Nix setup using Home-manager

{ config, pkgs, inputs, nixgl, user, ... }:

{
  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";

    packages = with pkgs;
      [
        (import nixgl { inherit pkgs; }).nixGLIntel # OpenGL for GUI apps.

        # pkgs.emacs
      ];

    activation = {
      linkDesktopApplications = {
        after = [ "writeBoundary" "createXdgUserDirectories" ];
        before = [ ];
        data = ''
          rm -rf ${config.xdg.dataHome}/"applications/home-manager"
          mkdir -p ${config.xdg.dataHome}/"applications/home-manager"
          cp -Lr ${config.home.homeDirectory}/.nix-profile/share/applications/* ${config.xdg.dataHome}/"applications/home-manager/"
        '';
      };
    };

    stateVersion = "23.05";

  };

  programs = { home-manager.enable = true; };

  nix = {
    settings.auto-optimise-store = true;

    package = pkgs.nixFlakes;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
    '';
  };

  nixpkgs.config.allowUnfree = true;
}
