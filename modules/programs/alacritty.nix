{
  custom-config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = custom-config.alacritty;

  colors = import ../themes/colors.nix;
in {
  options.alacritty = {
    enable = mkEnableOption {
      description = "Enables Alacritty terminal emulator";
      type = types.bool;
      default = false;
    };
    fontFamily = mkOption {
      description = "Set font family";
      type = types.str;
      default = "";
    };
    fontSize = mkOption {
      description = "Set font size";
      type = types.number;
      default = 12;
    };
  };

  config = mkIf cfg.enable {
    programs.alacritty = {
      inherit (cfg) enable;

      settings = {
        window = {
          dynamic_title = true;
          dynamic_padding = true;
          decorations = "full";
          opacity = 1;
        };
        selection = {
          save_to_clipboard = false;
        };
        scrolling = {
          history = 50000;
          multiplier = 2;
        };
        font = {
          normal = {family = cfg.fontFamily;};
          bold = {family = cfg.fontFamily;};
          italic = {family = cfg.fontFamily;};
          offset = {
            x = 0;
            y = 0;
          };
          glyph_offset = {
            x = 0;
            y = 0;
          };
          size = cfg.fontSize;
        };
        draw_bold_text_with_bright_colors = true;
        colors = with colors.scheme.dracula; {
          primary = {
            background = "${background}";
            foreground = "${foreground}";
            bright_foreground = "${white}";
          };
          cursor = {
            text = "${background}";
            curor = "${foreground}";
          };
          vi_mode_cursor = {
            text = "CellBackground";
            cursor = "CellForeground";
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
            white = "${white}";
          };
          bright = {
            black = "#6272a4";
            red = "#ff6e6e";
            green = "#69ff94";
            yellow = "#ffffa5";
            blue = "#d6acff";
            magenta = "#ff92df";
            cyan = "#a4ffff";
            white = "#ffffff";
          };
          search = {
            matches = {
              background = "${green}";
              foreground = "${current_line}";
            };
            focused_match = {
              background = "${orange}";
              foreground = "${current_line}";
            };
          };
          footer_bar = {
            background = "${background}";
            foreground = "${foreground}";
          };
          hints = {
            start = {
              background = "${yellow}";
              foreground = "${background}";
            };
            end = {
              background = "${background}";
              foreground = "${yellow}";
            };
          };
        };
      };
    };
  };
}
