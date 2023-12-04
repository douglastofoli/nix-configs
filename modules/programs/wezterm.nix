{
  custom-config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = custom-config.wezterm;

  colors = import ../themes/colors.nix;
in {
  options.wezterm = {
    enable = mkEnableOption {
      description = "Enables Wezterm terminal emulator";
      type = types.bool;
      default = false;
    };
    enableBashIntegration = mkOption {
      description = "Enables support for Bash";
      type = types.bool;
      default = false;
    };
    enableZshIntegration = mkOption {
      description = "Enables support for Zsh";
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
    programs.wezterm = {
      inherit (cfg) enable enableBashIntegration enableZshIntegration;

      colorSchemes = import ../themes/colors-wezterm.nix {inherit colors;};

      extraConfig = ''
        return {
          font = wezterm.font("${cfg.fontFamily}"),
          font_size = ${toString cfg.fontSize},
          line_height = 1.15,
          dpi = 96.0,
          bold_brightens_ansi_colors = true,
          audible_bell = "Disabled",

          color_scheme = "dracula";

          -- Tab bar
          enable_tab_bar = true,
          tab_bar_at_bottom = true,
          use_fancy_tab_bar = false,
          hide_tab_bar_if_only_one_tab = true,
          show_tab_index_in_tab_bar = false
        }
      '';
    };
  };
}
