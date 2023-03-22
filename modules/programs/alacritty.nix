{ config, pkgs, ... }:

let colors = import ../themes/colors.nix;
in {
  programs.alacritty = {
    enable = true;
    settings = {
      env = { TERM = "xterm-256color"; };
      scrolling = { history = 10000; };
      draw_bold_text_with_bright_colors = true;
      font = rec {
        normal = {
          family = "SauceCodePro Nerd Font Mono";
          Style = "Regular";
        };
        bold = {
          family = "SauceCodePro Nerd Font Mono";
          style = "Bold";
        };
        italic = {
          family = "SauceCodePro Nerd Font Mono";
          style = "Italic";
        };
        bold_italic = {
          family = "SauceCodePro Nerd Font Mono";
          style = "Bold Italic";
        };
        offset = {
          x = 0;
          y = 1;
        };
      };
      window = {
        paddings = {
          x = 6;
          y = 6;
        };

        dynamic_padding = false;
        opacity = 1.0;

        title = "Alacritty";
        class = {
          instance = "Alacritty";
          general = "Alacritty";
        };
      };
      colors = with colors.scheme.catppuccin-macchiato; {
        primary = {
          background = "${base}";
          foreground = "${text}";
          dim_foreground = "${text}";
          bright_foreground = "${text}";
        };

        cursor = {
          text = "${base}";
          cursor = "${rosewater}";
        };

        vi_mode_cursor = {
          text = "${base}";
          cursor = "${lavender}";
        };

        search = {
          matchs = {
            foreground = "${base}";
            background = "${subtext0}";
          };
          focused_match = {
            foreground = "${base}";
            background = "${green}";
          };
          footer_bar = {
            foreground = "${base}";
            background = "${subtext0}";
          };
        };

        hints = {
          start = {
            foreground = "${base}";
            background = "${yellow}";
          };
          end = {
            foreground = "${base}";
            background = "${subtext0}";
          };
        };

        selection = {
          text = "${base}";
          background = "${rosewater}";
        };

        normal = {
          black = "${surface1}";
          red = "${red}";
          green = "${green}";
          yellow = "${yellow}";
          blue = "${blue}";
          magenta = "${pink}";
          cyan = "${teal}";
          white = "${subtext1}";
        };

        bright = {
          black = "${surface2}";
          red = "${red}";
          green = "${green}";
          yellow = "${yellow}";
          blue = "${blue}";
          magenta = "${pink}";
          cyan = "${teal}";
          white = "${subtext0}";
        };

        dim = {
          black = "${surface1}";
          red = "${red}";
          green = "${green}";
          yellow = "${yellow}";
          blue = "${blue}";
          magenta = "${pink}";
          cyan = "${teal}";
          white = "${subtext1}";
        };

        indexed_colors = [
          {
            index = 16;
            color = "${peach}";
          }
          {
            index = 17;
            color = "${rosewater}";
          }
        ];
      };
    };
  };
}
