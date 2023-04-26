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
        monitor = 0;
        follow = "mouse";
        shrink = "no";
        padding = 20;
        horizontal_padding = 20;

        width = 275;
        height = 100;
        offset = "10x50";
        origin = "top-right";

        frame_width = 2;
        separator_height = 2;
        frame_color = "#161320";
        separator_color = "#161320";

        sort = "no";
        font = "Ubuntu 10";
        markup = "full";
        format = ''
          <b>%s</b>
          %b'';
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = "no";
        show_indicators = "yes";

        icon_position = "left";
        max_icon_size = 60;
        sticky_history = "no";
        history_length = 6;
        title = "Dunst";
        class = "Dunst";
        corner_radius = 8;

        mouse_left_click = "close_current";
        mouse_middle_click = "do_action";
        mouse_right_click = "close_all";
      };

      urgency_low = {
        background = "${base}";
        foreground = "${text}";
        frame_color = "${blue}";
        timeout = 5;
      };

      urgency_normal = {
        background = "${base}";
        foreground = "${text}";
        frame_color = "${blue}";
        timeout = 10;
      };

      urgency_critical = {
        background = "${base}";
        foreground = "${text}";
        frame_color = "${peach}";
        timeout = 20;
      };
    };
  };

  home.packages = [ pkgs.libnotify ];

  xdg.configFile."dunst/icons".source = ../../dotfiles/dunst/icons;

  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source =
    "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
}
