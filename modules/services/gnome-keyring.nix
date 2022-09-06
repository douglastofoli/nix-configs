{ pkgs, ... }:

{
  security.pam.services = {
    login.enableGnomeKeyring = true;
    lightdm.enableGnomeKeyring = true;
  };

  services.gnome.gnome-keyring = {
    enable = true;
  };

  services.dbus.packages = with pkgs; [ gnome.gnome-keyring gcr ];
}