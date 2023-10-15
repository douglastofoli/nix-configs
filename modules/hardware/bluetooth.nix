{ config, lib, vars, ... }:

{
  config = {
    hardware = {
      enableAllFirmware = true;

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
            ReconnectAttempts = 7;
            ReconnectIntervals = "1,2,4,8,16,32,64";
          };
        };
      };
    };

    services.blueman.enable = config.hardware.bluetooth.enable;

    environment.etc = {
      "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text =
        "	bluez_monitor.properties = {\n		[\"bluez5.enable-sbc-xq\"] = true,\n		[\"bluez5.enable-msbc\"] = true,\n		[\"bluez5.enable-hw-volume\"] = true,\n		[\"bluez5.headset-roles\"] = \"[ hsp_hs hsp_ag hfp_hf hfp_ag ]\"\n	}\n";
    };

    home-manager.users.${vars.user} = {
      services.blueman-applet.enable = config.hardware.bluetooth.enable;
    };
  };
}
