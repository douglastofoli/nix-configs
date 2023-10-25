{ config, lib, vars, ... }:

{
  config = {
    hardware = {
      bluetooth = {
        enable = true;
        settings = {
          General = {
            Name = "Bluetooth";
            Enable = "Source,Sink,Media,Socket";
            DiscoverableTimeout = 0;
            AlwaysPairable = true;
            FastConnectable = true;
          };
          Policy = {
            AutoEnable = true;
            ReconnectAttempts = 5;
            ReconnectIntervals = "1,2,4,8,16";
          };
        };
      };

      enableAllFirmware = true;
    };

    services.blueman.enable = config.hardware.bluetooth.enable;

    home-manager.users.${vars.user} = {
      services.blueman-applet.enable = config.services.blueman.enable;
    };

    systemd.services.fix-generic-usb-bluetooth-dongle = {
      description = "Fixes for generic USB bluetooth dongle.";
      wantedBy = [ "post-resume.target" ];
      after = [ "post-resume.target" ];
      script = builtins.readFile ../../dotfiles/local/bin/bluetooth-reset;
      scriptArgs = "2357:0604"; # Vendor ID and Product ID here
      serviceConfig.Type = "oneshot";
    };
  };
}
