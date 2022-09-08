{ pkgs, ... }:

{
  security.pam = { 
    enableSSHAgentAuth = true;
    services.lightdm.enableGnomeKeyring = true;
    services.lightdm.gnupg.enable = true;
  };

  services.gnome.gnome-keyring = {
    enable = true;
  };
}