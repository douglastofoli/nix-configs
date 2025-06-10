{pkgs, ...}: {
  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };

  services.udev.packages = [pkgs.yubikey-personalization];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-gtk2;
  };

  environment.systemPackages = [
    pkgs.pinentry-gtk2
  ];
}
