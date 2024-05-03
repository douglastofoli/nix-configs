{
  config,
  lib,
  vars,
  ...
}: {
  home-manager.users.${vars.user} = lib.mkIf config.services.xserver.enable {
    programs.rofi.enable = true;

    home.file.".config/rofi" = {
      source = ../../dotfiles/rofi;
      recursive = true;
    };
  };
}
