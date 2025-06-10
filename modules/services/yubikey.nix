{pkgs, ...}: {
  # Ativa o Gnome Keyring via PAM
  security.pam.services.lightdm.enableGnomeKeyring = true;

  # Ativa suporte à YubiKey no modo challenge-response
  security.pam.yubico = {
    enable = true;
    mode = "challenge-response";
  };

  # Suporte a smartcard
  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  # Programas de gerenciamento
  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-personalization
    gnupg
    pinentry-gtk2
    gnome-keyring
  ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = false; # Gnome Keyring cuidará do SSH agent
      pinentryPackage = pkgs.pinentry-gtk2;
      settings = {
        default-cache-ttl = 86400;
        max-cache-ttl = 604800;
        default-cache-ttl-ssh = 86400;
        max-cache-ttl-ssh = 604800;
      };
    };

    ssh = {
      startAgent = false;
      extraConfig = ''
        AddKeysToAgent yes
      '';
    };
  };

  # Ativa o gnome-keyring para sessões de desktop
  services.gnome.gnome-keyring.enable = true;
}
