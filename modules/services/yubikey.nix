{ pkgs, ... }:

{
  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };

  services.udev.packages = [ pkgs.yubikey-personalization ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}
