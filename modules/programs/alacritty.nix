{ config, pkgs, ... }:

let colors = import ../themes/colors.nix;
in {
  programs.alacritty = {
    enable = true;
    #shell = { program = "${pkgs.zsh}/bin/zsh"; };
    settings = {
      env = { TERM = "xterm-256color"; };
      scrolling = { history = 10000; };
      font = rec {
        normal.family = "JetBrainsMono Nerd Font Mono";
        bold.family = "JetBrainsMono Nerd Font Mono";
        italic.family = "JetBrainsMono Nerd Font Mono";
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
      };
      colors = with colors.scheme.dracula; {
        primary = {
          background = "${background}";
          foreground = "${foreground}";
          bright_foreground = "${white}";
        };
        cursor = {
          text = "CellBackground";
          cursor = "CellForeground";
        };
        vi_mode_cursor = {
          text = "CellBackground";
          cursor = "CellForeground";
        };
        search = {
          matches = {
            foreground = "${current_line}";
            background = "${green}";
          };
          focused_match = {
            foreground = "${current_line}";
            background = "${orange}";
          };
        };
        footer_bar = {
          background = "${background}";
          foreground = "${foreground}";
        };
        hints = {
          start = {
            foreground = "${background}";
            background = "${yellow}";
          };
          end = {
            foreground = "${yellow}";
            background = "${background}";
          };
        };
        line_indicator = {
          foreground = "None";
          background = "None";
        };
        selection = {
          text = "CellForeground";
          background = "${current_line}";
        };
        normal = {
          black = "${black}";
          red = "${red}";
          green = "${green}";
          yellow = "${yellow}";
          blue = "${purple}";
          magenta = "${pink}";
          cyan = "${cyan}";
          white = "${foreground}";
        };
        bright = {
          black = "${comment}";
          red = "#ff6e6e";
          green = "#69ff94";
          yellow = "#ffffa5";
          blue = "#d6acff";
          magenta = "#ff92df";
          cyan = "#a4ffff";
          white = "${white}";
        };
      };
    };
  };
}
