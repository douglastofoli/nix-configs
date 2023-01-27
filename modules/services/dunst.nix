{ config, lib, pkgs, ... }:

let
  colors = import ../themes/colors.nix;
in {
  services.dunst = {
    enable = true;
    iconTheme = { # Icons
      name = "Papirus Dark";
      package = pkgs.papirus-icon-theme;
      size = "16x16";
    };
    settings = with colors.scheme.dracula; {
      global = {
        monitor = 0;
        follow = "mouse";
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
        indicate_hidden = "yes";
        transparency = 15;
        separator_height = 1;
        padding = 8;
        horizontal_padding = 10;
        text_icon_padding = 0;
        frame_width = 0;
        frame_color = "${background}";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        font = "RobotoMono Nerd Font 10";
        line_height = 0;
        markup = "full";
        format = "%s %p\n%b";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "left";
        min_icon_size = 0;
        max_icon_size = 64;
        icon_path = "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";
        sticky_history = "yes";
        history_length = 20;
        demnu = "/usr/bin/dmenu -p dunst:";
        browser = "/usr/bin/firefox -new-tab";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        corner_radius = 0;
        ignore_dbusclose = false;
        force_xwayland = false;
        force_xinerama = false;
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
      };
      experimental = {
        per_monitor_dpi = false;
      };
      urgency_low = {
        background = "${background}";
        foreground = "${comment}";
        timeout = 10;
      };
      urgency_normal = {
        background = "${background}";
        foreground = "${purple}";
        timeout = 10;
      };
      urgency_critical = {
        background = "${red}";
        foreground = "${foreground}";
        frame_color = "${red}";
        timeout = 0;
      };
    };
  };

  home.packages = with pkgs; [
    libnotify
  ];

  xdg.dataFile."dbus-1/services/org.knopwob.dunst.service".source = "${pkgs.dunst}/share/dbus-1/services/org.knopwob.dunst.service";
}
