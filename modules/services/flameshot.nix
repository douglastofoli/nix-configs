{ config, lib, pkgs, vars, ... }:

{
  config = lib.mkIf config.services.xserver.enable {
    home-manager.users.${vars.user} = {
      services.flameshot = {
        enable = true;
        settings = {
          General = {
            savePath = "/home/${vars.user}/Pictures/Screenshots";
            saveAsFileExtension = ".png";
            uiColor = "#bd93f9";
            showHelp = "false";
            disabledTrayIcon = "true";
          };
        };
      };
    };
  };
}
