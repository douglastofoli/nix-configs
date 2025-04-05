{pkgs, ...}: let
  colors = import ../themes/colors.nix;
in {
  home.packages = with pkgs; [wofi wofi-emoji wofi-pass];

  xdg.configFile = {
    "wofi/config".text = ''
      width=800
      height=400
      location=center
      show=drun
      prompt=Search...
      filter_rate=100
      allow_markup=true
      no_actions=true
      halign=fill
      orientation=vertical
      content_halign=fill
      insensitive=true
      allow_images=true
      image_size=28
      gtk_dark=true
    '';

    "wofi/style.css".text = with colors.scheme.catppuccin-macchiato; ''
      @define-color clear rgba(0, 0, 0, 0.0);
      @define-color primary rgba(0, 0, 0, 0.75);

      window {
        margin: 2px;
        border: 2px solid;
        border-color: ${surface0};
        background-color: ${base};
        border-radius: 10px;
        font-family: "Ubuntu Nerd Font";
        font-size: 14px;
      }

      #input {
        padding: 2px;
        margin: 4px;
        margin-bottom: 8px;
        border: none;
        color: @foreground;
        background-color: ${surface0};
        outline: none;
      }

      #inner-box {
        margin: 2px;
        border: 0px solid;
        background-color: @clear;
        border-radius: 8px;
      }

      #outer-box {
        margin: 5px;
        border: none;
        border-radius: 8px;
        background-color: @clear;
      }

      #scroll {
        margin: 0px;
        border: none;
      }

      #text:selected {
        color: @foreground;
        margin: 0px 0px;
        border: none;
        border-radius: 8px;
      }

      #entry {
        margin: 0px 0px;
        border: none;
        border-radius: 0px;
        background-color: transparent;
      }

      #entry:selected {
        margin: 0px 0px;
        border: none;
        border-radius: 8px;
        background-color: ${surface2};
      }
    '';
  };
}
