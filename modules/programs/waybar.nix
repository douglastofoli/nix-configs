{ config, lib, pkgs, user, ... }:

let
  waybarStyle = ''
    * {
      border: none;
      border-radius: 0;
      font-family: monospace, "Font Awesome 6 Free Solid";
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
      background: #a6e3a1;
      border-radius: 10px;
    }
    #workspaces button:hover {
      background: #11111b;
      color: #cdd6f4;
      border-radius: 10px;
    }
    #language,
    #window,
    #clock,
    #pulseaudio,
    #network,
    #workspaces,
    #tray,
    #backlight {
      background: #1e1e2e;
      padding: 0px 10px;
      margin: 3px 0px;
      margin-top: 10px;
      border: 1px solid #181825;
    }
    #tray {
      border-radius: 10px;
      margin-right: 10px;
    }
    #workspaces {
      background: #1e1e2e;
      border-radius: 10px;
      margin-left: 10px;
      padding-right: 0px;
      padding-left: 5px;
    }
    #language {
      color: #f38ba8;
      border-radius: 10px 0px 0px 10px;
      border-right: 0px;
      margin-left: 10px;
    }
    #window {
      border-radius: 10px;
      margin-left: 60px;
      margin-right: 60px;
    }
    #clock {
      color: #f9e2af;
      border-radius: 10px;
      margin-left: 10px;
    }
    #network {
      color: #fab387;
      border-left: 0px;
      border-right: 0px;
    }
    #pulseaudio {
      color: #89b4fa;
      border-radius: 0px 10px 10px 0px;
      border-left: 0px;
      margin-right: 10px;
    }
    #pulseaudio.muted {
      color: #313244;
    }
    #backlight {
      color: #cba6f7;
      border-left: 0px;
      border-right: 0px;
    }
  '';

  waybarConf = [{
    "layer" = "top";
    "position" = "top";
    "height" = 0;

    "modules-left" = [ "clock" "wlr/workspaces" ];
    "modules-center" = [ "hyprland/window" ];
    "modules-right" =
      [ "tray" "hyprland/language" "network" "backlight" "pulseaudio" ];

    "clock" = {
      "format" = "{: %R   %d/%m}";
      "tooltip-format" = ''
        <big>{:%Y %B}</big>
        <tt><small>{calendar}</small></tt>'';
    };
    "wlr/workspaces" = {
      "disable-scroll" = true;
      "all-outputs" = true;
      "on-click" = "activate";
      "persistent_workspaces" = {
        "1" = [ ];
        "2" = [ ];
        "3" = [ ];
        "4" = [ ];
        "5" = [ ];
        "6" = [ ];
        "7" = [ ];
        "8" = [ ];
        "9" = [ ];
        "10" = [ ];
      };
    };
    "tray" = {
      "icon-size" = 13;
      "spacing" = 10;
    };
    "hyprland/language" = { "format" = " {}"; };
    "network" = {
      "format-wifi" = " {essid}";
      "format-ethernet" = " {ipaddr}/{cidr}";
      "format-linked" = "{ifname} (No IP) ";
      "format-disconnected" = " Disconnected";
      "tooltip-format-wifi" =
        "Signal Strenght: {signalStrength}% | Down Speed: {bandwidthDownBits}, Up Speed: {bandwidthUpBits}";
      "on-click" = "wofi-wifi-menu";
    };
    "backlight" = {
      "device" = "intel_backlight";
      "format" = "{icon} {percent}%";
      "format-icons" = [ "" "" "" ];
      "on-scroll-up" = "brightnessctl set 1%+";
      "on-scroll-down" = "brightnessctl set 1%-";
      "min-length" = 6;
    };
    "pulseaudio" = {
      "format" = "{icon} {volume}% {format_source}";
      "format-muted" = " Muted {format_source}";
      "format-bluetooth" = " {volume}% {format_source}";
      "format-bluetooth-muted" = " Muted {format_source}";
      "tooltip-format" = "{icon} {desc}";
      "format-source" = " {volume}%";
      "format-source-muted" = "";
      "on-click" = "pavucontrol";
      "on-click-middle" = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
      "scroll-step" = 1;
      "format-icons" = {
        "headphone" = "";
        "hands-free" = "";
        "headset" = "";
        "phone" = "";
        "portable" = "";
        "car" = "";
        "default" = [ "" "" "" ];
      };
    };
  }];
in {
  environment.systemPackages = with pkgs; [ waybar ];

  nixpkgs.overlays = [
    (self: super: {
      waybar = super.waybar.overrideAttrs (oldAttrs: {
        mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
        patchPhase = ''
          substituteInPlace src/modules/wlr/workspace_manager.cpp --replace "zext_workspace_handle_v1_activate(workspace_handle_);" "const std::string command = \"hyprctl dispatch workspace \" + name_; system(command.c_str());"
        '';
      });
    })
  ];

  home-manager.users.${user} = {
    programs.waybar = {
      enable = true;
      style = waybarStyle;
      settings = waybarConf;
    };
  };
}
