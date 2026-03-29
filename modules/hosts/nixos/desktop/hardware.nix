{
  flake.modules.nixos.desktop =
    {
      config,
      lib,
      modulesPath,
      pkgs,
      ...
    }:
    {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
      ];

      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usbhid"
        "usb_storage"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModulePackages = [ ];

      boot = {
        loader = {
          efi = {
            canTouchEfiVariables = true;
          };

          grub = {
            enable = true;
            efiSupport = true;
            devices = [ "nodev" ];
            useOSProber = true;
          };

          timeout = 3;
        };
      };

      powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

      hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

      hardware = {
        graphics = {
          enable = true;
          extraPackages = with pkgs; [
            mesa
            intel-media-driver
            intel-vaapi-driver
            libva-vdpau-driver
            libvdpau-va-gl
          ];
        };
      };
    };
}
