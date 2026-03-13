{
  flake.modules.nixos.desktop =
    { config, lib, ... }:
    {
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = config.host.name;
        networkmanager.enable = true;
      };
    };
}
