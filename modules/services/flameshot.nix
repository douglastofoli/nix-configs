{ pkgs, user, ... }:

{
  services.flameshot = {
    enable = true;
    settings = {
      General = {
        savePath = "/home/${user}/gdrive/Screenshots";
        saveAsFileExtension = ".png";
        uiColor = "#2d0096";
        showHelp = "false";
        disabledTrayIcon = "true";
      };
    };
  };
}
