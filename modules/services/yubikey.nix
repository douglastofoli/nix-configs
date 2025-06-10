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

    settings = {
      default-cache-ttl = 86400;
      max-cache-ttl = 604800;
      default-cache-ttl-ssh = 86400;
      max-cache-ttl-ssh = 604800;
    };
  };

  environment.systemPackages = [
    pkgs.pinentry-gtk2
  ];
}
