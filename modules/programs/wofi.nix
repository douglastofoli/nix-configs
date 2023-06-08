{ config, lib, pkgs, ... }:

let
  inherit (config.lib.formats.rasi) mkLiteral;
  colors = import ../themes/colors.nix;
in
{
  home.packages = with pkgs; [
    wofi
    wofi-emoji
  ];

  xdg.configFile = {
    "wofi/config".text = ''
       width=280
       lines=10
       xoffset=5
       yoffset=5
       location=1
       prompt=Search...
       filter_rate=100
       allow_markup=false
       no_actions=true
       halign=fill
       orientation=vertical
       content_halign=fill
       insensitive=true
       allow_images=true
       image_size=20
       hide_scroll=true
    '';

 #   "wofi/style.css".text = with colors.schema.catppuccin-macchiato; {

 #   };
  };
}
