{ lib, ... }:

let
  colors = import ../themes/colors.nix;
in
{
  xdg.configFile = {
    "waybar/config".text = ''
      {
        "layer": "top",
        "position": "top",
        "height": 0,

        "modules-left": [
          "wlr/workspaces",
        ],

        "modules-center": [
          "hyprland/window",
        ],

        "modules-right": [
          "cpu",
          "memory",
          "pulseaudio",
          "network",
          "clock",
          "tray",
        ],

        "wlr/workspaces": {
          "all-outputs": false,
          "active-only": false,
          "on-click": "activate",
          "format": "{icon}",
          "on-scroll-up": "hyprctl dispatch workspace e+1",
          "on-scroll-down": "hyprctl dispatch workspace e-1",
        },

        "hyprland/window": {
          "format": "{}",
        },

        "cpu": {
          "format": " {}%",
          "max-length": 10,
          "on-click": "$TERMINAL -e btop",
        },

        "memory": {
          "format": " {}%",
        },
 
        "network": {
          "format-wifi": "  {essid}",
          "format-ethernet": " {essid} Connected",
          "format-linked": "{ifname} (No IP) ",
          "format-disconnected": "",
          "tooltip-format-wifi": " {ifname} @ {essid}\nIP: {ipaddr}\nStrength: {signalStrength}%\nFreq: {frequency}MHz\nUP {bandwidthUpBits} DOWN {bandwidthDownBits}",
          "on-click": "wofi-wifi-menu",
        },

        "pulseaudio": {
          "format": "{icon} {volume}%",
          "format-muted": "󰝟 Muted",
          "on-click": "amixer set Master toggle",
          "scroll-step": 1,
          "format-icons": {
            "headphone": "󰋋",
            "hands-free": "󰋋",
            "headset": "󰋋",
            "phone": "",
            "portable": "",
            "car": "",
            "default": ["", "", "", ""]
          },
        },

        "clock": {
          "format": "{:󰥔 %R  󰃭 %d/%m}",
          "tooltip-format": "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>",
        },

        "tray": {
          "icon-size": 13,
          "spacing": 10,
        },
      }
    '';

    "waybar/style.css".text = with colors.scheme.catppuccin-macchiato; ''
      * {
        border: none;
        border-radius: 0;
        font-family: "JetBrainsMono Nerd Font";
        font-weight: bold;
        font-size: 14px;
        min-height: 0;
      }

      window#waybar {
        background: rgba(21, 18, 27, 0);
        color: #cdd6f4;
      }

      tooltip {
        background: #1e1e2e;
        border-radius: 10px;
        border-width: 2px;
        border-style: solid;
        border-color: #11111b;
      }

      #workspaces button {
        padding: 5px;
        color: #313244;
        margin-right: 5px;
      }

      #workspaces button.active {
        color: #a6adc8;
      }

      #workspaces button.focused {
        color: #a6adc8;
        background: #eba0ac;
        border-radius: 10px;
      }

      #workspaces button.urgent {
        color: #11111b;
        background: ${red};
        border-radius: 10px;
      }

      #workspaces button:hover {
        background: #11111b;
        color: #cdd6f4;
        border-radius: 10px;
      }

      #workspaces,
      #window,
      #cpu,
      #memory,
      #language,
      #pulseaudio,
      #network,
      #clock,
      #tray {
        background: #1e1e2e;
        padding: 0px 10px;
        margin: 3px 0px;
        margin-top: 10px;
        border: 1px solid ${crust};
      }

      #workspaces {
        background: #1e1e2e;
        border-radius: 10px;
        margin-left: 10px;
        padding-right: 0px;
        padding-left: 5px;
      }

      #window {
        border-radius: 10px;
        margin-left: 60px;
        margin-right: 60px;
      }

      #cpu {
        color: ${sky};
        border-radius: 10px 0px 0px 10px;
        border-left: 10px;
        border-right: 0px;
      }

      #memory {
        color: ${green};
        border-left: 0px;
        border-right: 0px;
      }

      #language {
        color: ${lavender};
        border-left: 0px;
        border-right: 0px;
      }

      #pulseaudio {
        color: ${blue};
        border-left: 0px;
        border-right: 0px;
      }

      #network {
        color: ${peach};
        border-left: 0px;
        border-right: 0px;
      }

      #clock {
        color: ${mauve};
        border-radius: 0px 10px 10px 0px;
        margin-right: 10px;
        border-left: 0px;
      }

      #tray {
        border-radius: 10px;
        margin-right: 10px;
      }
    '';
  };
}
