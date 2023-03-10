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
        follow = "none";
        width = 300;
        height = 300;
        origin = "top-right";
        offset = "10x50";
        scale = 0;
        notification_limit = 0;
        progress_bar = true;
        progress_bar_height = 10;
        progress_bar_frame_width = 1;
        progress_bar_min_width = 150;
        progress_bar_max_width = 300;
        progress_bar_corner_radius = 0;
        indicate_hidden = "yes";
        transparency = 0;
        separator_height = 1;
        padding = 8;
        horizontal_padding = 8;
        text_icon_padding = 0;
        frame_width = 2;
        frame_color = "${blue}";
        gap_size = 4;
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        font = "Ubuntu 10";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b> %p\\n%b";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "left";
        min_icon_size = 32;
        max_icon_size = 128;
        icon_path =
          "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";
        sticky_history = "yes";
        history_length = 20;
        dmenu = "/usr/bin/dmenu -p dunst:";
        browser = "/run/current-system/sw/bin/xdg-open";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        corner_radius = 8;
        ignore_dbusclose = false;
        force_xwayland = false;
        force_xinerama = false;
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
      };
      experimental = { per_monitor_dpi = false; };
      urgency_low = {
        background = "${base}";
        foreground = "${text}";
        timeout = 10;
      };
      urgency_normal = {
        background = "${base}";
        foreground = "${text}";
        timeout = 10;
      };
      urgency_critical = {
        background = "${base}";
        foreground = "${text}";
        frame_color = "${peach}";
        timeout = 0;
      };
    };
  };

  home.packages = [ pkgs.libnotify ];

  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source = "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
}
