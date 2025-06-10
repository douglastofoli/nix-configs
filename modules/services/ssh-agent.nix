{ config, lib, vars, ... }:
{
  config = {
    home-manager.users.${vars.user}.programs.ssh = {
      enable = true;
      startAgent = true;
      extraConfig = ''
        AddKeysToAgent yes
      '';
    };
  };
}
