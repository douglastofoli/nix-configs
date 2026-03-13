{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      security.pam.yubico = {
        enable = true;
        mode = "challenge-response";
      };

      programs = {
        gnupg.agent = {
          enable = true;
          enableSSHSupport = false;
          pinentryPackage = pkgs.pinentry-curses;
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

      services.gnome.gnome-keyring.enable = true;
    };
}
