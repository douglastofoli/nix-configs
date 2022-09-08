{ pkgs, ... }:

{
  security.pam = { 
    enableSSHAgentAuth = true;
    services.lightdm.enableGnomeKeyring = true;
  };

  services.gnome.gnome-keyring = {
    enable = true;
  };
}