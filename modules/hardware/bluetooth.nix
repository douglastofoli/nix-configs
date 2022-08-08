{
  hardware.bluetooth = {
    enable = true;
    hsphfpd.enable = true; # HSP & HFP daemon
    settings = {
      General = {
        Name = "nix-bluetooth";
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
