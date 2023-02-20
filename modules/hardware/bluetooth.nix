{ config, ... }:

{
  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Name = "doug-bluetooth";
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
}
