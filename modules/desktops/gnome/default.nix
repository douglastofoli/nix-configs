{
  config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
in {
  options.gnome = {
    enable = mkEnableOption {
      description = "Enables Gnome";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.gnome.enable {
    services.xserver = {
      enable = true;

      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
      desktopManager.gnome.enable = true;

      xkb = {
        layout = "br,us";
        options = "grp:alt_space_toggle";
      };

      excludePackages = [];
    };
  };
}
