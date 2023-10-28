{ config, lib, vars, ... }:

{
  config = {
    hardware = {
      bluetooth = {
        enable = true;
        powerOnBoot = true;
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
    };

    services.blueman.enable = config.hardware.bluetooth.enable;

    home-manager.users.${vars.user} = {
      services.blueman-applet.enable = config.services.blueman.enable;
    };
  };
}
