{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      env = { TERM = "xterm-256color"; };
      scrolling = { history = 5000; };
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
      shell = { program = "${pkgs.zsh}/bin/zsh"; };
      colors = {
        primary = {
          background = "#1E1E2E";
          foreground = "#CDD6F4";
          dim_foreground = "#CDD6F4";
          bright_foreground = "#CDD6F4";
        };
        cursor = {
          text = "#1E1E2E";
          cursor = "#F5E0DC";
        };
        vi_mode_cursor = {
          text = "#1E1E2E";
          cursor = "#B4BEFE";
        };
        search = {
          matches = {
            background = "#A6ADC8";
            foreground = "#1E1E2E";
          };
          focused_match = {
            background = "#A6E3A1";
            foreground = "#1E1E2E";
          };
          bar = {
            background = "#A6ADC8";
            foreground = "#1E1E2E";

          };
        };
        hints = {
          start = {
            background = "#F9E2AF";
            foreground = "#1E1E2E";
          };
          end = {
            background = "#A6ADC8";
            foreground = "#1E1E2E";
          };
        };
        selection = {
          text = "#1E1E2E";
          background = "#F5E0DC";
        };
        normal = {
          black = "#45475A";
          red = "#F38BA8";
          green = "#A6E3A1";
          yellow = "#F9E2AF";
          blue = "#89B4FA";
          magenta = "#F5C2E7";
          cyan = "#94E2D5";
          white = "#BAC2DE";
        };
        bright = {
          black = "#585B70";
          red = "#F38BA8";
          green = "#A6E3A1";
          yellow = "#F9E2AF";
          blue = "#89B4FA";
          magenta = "#F5C2E7";
          cyan = "#94E2D5";
          white = "#A6ADC8";
        };
        dim = {
          black = "#45475A";
          red = "#F38BA8";
          green = "#A6E3A1";
          yellow = "#F9E2AF";
          blue = "#89B4FA";
          magenta = "#F5C2E7";
          cyan = "#94E2D5";
          white = "#BAC2DE";
        };
      };
    };
  };
}
