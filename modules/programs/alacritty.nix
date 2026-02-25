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
    };
  };
}
