{
  flake.modules.nixos.base = {
    programs.gnupg= {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "tty";
    };
  };
}
