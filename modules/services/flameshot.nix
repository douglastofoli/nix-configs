{ pkgs, user, ... }:

{
  services.flameshot = {
    enable = true;
    settings = {
      General = {
        savePath = "/home/${user}/GoogleDrive/Screenshots";
        saveAsFileExtension = ".png";
        uiColor = "#bd93f9";
        showHelp = "false";
        disabledTrayIcon = "true";
      };
    };
  };
}
