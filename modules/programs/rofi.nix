{
  config,
  lib,
  vars,
  ...
}: {
  home-manager.users.${vars.user} = lib.mkIf (config.services.xserver.enable || config.hyprland.enable) {
    programs.rofi.enable = true;
  };
}
