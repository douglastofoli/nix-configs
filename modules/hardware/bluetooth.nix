{ config, ... }:

{
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Name = "nixos-bluetooth";
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
}
