{
  config,
  lib,
  vars,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
in {
  options.docker = {
    enable = mkEnableOption {
      description = "Enables Docker virtualisation";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.docker.enable {
    virtualisation.docker = {
      enable = true;
      enableOnBoot = true;
    };

    users.extraGroups.docker.members = ["${vars.user}"];
  };
  #environment.systemPackages = with pkgs; [ docker docker-compose ];
}
