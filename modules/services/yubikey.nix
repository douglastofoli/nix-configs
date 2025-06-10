{pkgs, ...}: {
  security.pam.services.lightdm.enableGnomeKeyring = true;

  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };

  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  programs = {
    gnupg.agent = {
      enable = false;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-gtk2;
      settings = {
        default-cache-ttl = 86400;
        max-cache-ttl = 604800;
        default-cache-ttl-ssh = 86400;
        max-cache-ttl-ssh = 604800;
      };
    };

    ssh = {
      startAgent = true;
      extraConfig = ''
        AddKeysToAgent yes
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-personalization
    gnupg
    pinentry-gtk2
  ];
}
