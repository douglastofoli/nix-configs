{ config, ... }:

{
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    wireplumber.enable = false;
    media-session.enable = true;
    config.pipewire = { "default.metadata" = true; };
    media-session.config.bluez-monitor.rules = [
      {
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profile" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            "bluez5.msbc-support" = true;
          };
        };
      }
      {
        matches = [
          { "node.name" = "~bluez_input.*"; }
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = { "node.pause-on-idle" = true; };
      }
    ];
  };
}
