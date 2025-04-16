{
  custom-config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = custom-config.alacritty;
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
        env.TERM = "xterm-256color";

        terminal.shell = {
          program = "${pkgs.tmux}/bin/tmux";
          args = [
            "new-session"
            "-A"
            "-D"
            "-s"
            "main"
          ];
        };
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

        colors = {
          primary = {
            background = "#282a36";
            foreground = "#f8f8f2";
            bright_foreground = "#ffffff";
          };
          cursor = {
            text = "#282a36";
            cursor = "#f8f8f2";
          };
          vi_mode_cursor = {
            text = "CellBackground";
            cursor = "CellForeground";
          };
          selection = {
            text = "CellForeground";
            background = "#44475a";
          };
          normal = {
            black = "#21222c";
            red = "#ff5555";
            green = "#50fa7b";
            yellow = "#f1fa8c";
            blue = "#bd93f9";
            magenta = "#ff79c6";
            cyan = "#8be9fd";
            white = "#f8f8f2";
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
              background = "#50fa7b";
              foreground = "#44475a";
            };
            focused_match = {
              background = "#ffb86c";
              foreground = "#44475a";
            };
          };
          footer_bar = {
            background = "#282a36";
            foreground = "#f8f8f2";
          };
          hints = {
            start = {
              background = "#f1fa8c";
              foreground = "#282a36";
            };
            end = {
              background = "#282a36";
              foreground = "#f1fa8c";
            };
          };
        };
      };
    };
  };
}
