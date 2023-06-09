{ config, lib, pkgs, ... }:

let colors = import ../themes/colors.nix;
in {
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Papirus Dark";
      package = pkgs.papirus-icon-theme;
      size = "16x16";
    };
    settings = with colors.scheme.catppuccin-macchiato; {
      global = {
        follow = "mouse";
        indicate_hidden = true;

        offset = "10x10";

        notification_height = 0;
        separator_height = 0;

        padding = 8;
        horizontal_padding = 8;
        text_icon_padding = 0;
        frame_width = 2;

        frame_color = "${blue}";
        separator_color = "frame";

        sort = "yes";
        iddle_threshold = 120;
        font = "Ubuntu Nerd Font 10";
        line_height = 0;
        markup = "full";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        word_wrap = "yes";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";

        min_icon_size = 0;
        max_icon_size = 64;

        icon_path = "${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/status/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/devices/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/actions/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/animations/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/apps/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/categories/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/emblems/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/emotes/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/devices/mimetypes:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/17x16/panel/:${pkgs.catppuccin-papirus-folders}/share/icons/Papirus-Dark/16x16/places/";

        dmenu = "${pkgs.wofi}/bin/wofi -p dunst:";
        browser = "${pkgs.firefox}/bin/firefox --new-tab";

        title = "Dunst";
        class = "Dunst";

        corner_radius = 10;
        timeout = 5;
      };

      urgency_low = {
        background = "${base}";
        foreground = "${text}";
      };

      urgency_normal = {
        background = "${base}";
        foreground = "${text}";
      };

      urgency_critical = {
        background = "${base}";
        foreground = "${text}";
        frame_color = "${peach}";
      };
    };
  };

  home.packages = [ pkgs.libnotify ];

  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source =
    "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
}
