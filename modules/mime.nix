{
  flake.modules.homeManager.mime =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      xdg = {
        mime.enable = true;
        mimeApps = {
          enable = true;
          defaultApplications = {
            "image/jpeg" = [
              "image-roll.desktop"
              "feh.desktop"
            ];
            "image/png" = [
              "image-roll.desktop"
              "feh.desktop"
            ];
            "text/plain" = "nvim-kitty.desktop";
            "text/html" = "nvim-kitty.desktop";
            "text/csv" = "nvim-kitty.desktop";
            "application/pdf" = [
              "firefox.desktop"
              "google-chrome.desktop"
            ];
            "application/zip" = "org.gnome.FileRoller.desktop";
            "application/x-tar" = "org.gnome.FileRoller.desktop";
            "application/x-bzip2" = "org.gnome.FileRoller.desktop";
            "application/x-gzip" = "org.gnome.FileRoller.desktop";
            "x-scheme-handler/http" = [
              "firefox.desktop"
              "google-chrome.desktop"
            ];
            "x-scheme-handler/https" = [
              "firefox.desktop"
              "google-chrome.desktop"
            ];
            "x-scheme-handler/about" = [
              "firefox.desktop"
              "google-chrome.desktop"
            ];
            "x-scheme-handler/unknown" = [
              "firefox.desktop"
              "google-chrome.desktop"
            ];
            "x-scheme-handler/mailto" = [ "gmail.desktop" ];
            "audio/mp3" = "mpv.desktop";
            "audio/x-matroska" = "mpv.desktop";
            "video/webm" = "mpv.desktop";
            "video/mp4" = "mpv.desktop";
            "video/x-matroska" = "mpv.desktop";
            "inode/directory" = "pcmanfm.desktop";
          };
        };
      };
    };
}
