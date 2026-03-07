{
  flake.modules.nixos.desktop = {
    fileSystems."/" = {
      device = "/dev/disk/by-uuid/d4c23be3-b152-4137-9aee-149d0225db7e";
      fsType = "ext4";
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/E9BA-5F35";
      fsType = "vfat";
      options = [
        "fmask=0077"
        "dmask=0077"
      ];
    };

    swapDevices = [ ];
  };
}
