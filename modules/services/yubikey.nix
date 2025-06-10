{pkgs, ...}: {
  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };

  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-personalization
    gnupg
    pinentry-gtk2
  ];
}
