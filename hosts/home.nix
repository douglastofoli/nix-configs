# General Home-manager configuration

{ config, lib, pkgs, user, ... }:

{
  imports = (import ../modules/programs);

  home = {
    username = "${user}";
    homeDirectory = "/home/${user}";
  };
}
