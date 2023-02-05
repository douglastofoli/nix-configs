{ config, lib, pkgs, user, ... }:

let
  waybarStyle = ''
    * {
      font-family: "JetBrainsMono Nerd Font";
      font-size: 12px;
      font-weight: bold;
      border-radius: 0px;
      transition-property: background-color;
      transition-duration: 0.5s;
    }
    @keyframes blind_red {
      to {
        background-color: rgb(242, 143, 173);
        color: rgb(26, 24, 38);
      }
    }
    .warning, .critical, .urgent {
      animation-name: blink_red;
      animation-duration: 1s;
      animation-timing-function: linear;
      animation-iteration-count: infinite;
      animation-direction: alternate;
    }
    window#waybar {
      background-color: transparent;
    }
    window > box {
      margin-left: 5px;
      margin-right: 5px;
      margin-top: 5px;
      background-color: rgb(30, 30, 46);
    }
    #workspaces {
      padding-left: 0px;
      padding-right: 4px;
    }
    #workspaces button {
      padding-top: 5px;
      padding-bottom: 5px;
      padding-left: 6px;
      padding-right: 6px;
    }
    #workspaces button.active {
      background-color: rgb(181, 232, 224);
      color: rgb(26, 24, 38);
    }
    #workspaces button.urgent {
      color: rgb(26, 24, 38);
    }
    #workspaces button:hover {
      background-color: rgb(248, 189, 150);
      color: rgb(26, 24, 38);
    }
    tooltip {
      background: rgb(48, 45, 65);
    }
    tooltip label {
      color: rgb(217, 224, 238);
    }
    #custom-launcher {
      font-size: 20px;
      padding-left: 8px;
      padding-right: 6px;
      color: #7ebae4;
    }
    #mode, #clock, #memory, #temperature, #cpu, #mpd, #custom-wall, #backlight, #pulseaudio, #network, #battery, #custom-powermenu, #custom-cava-internal {
      padding-left: 10px;
      padding-right: 10px;
    }
    #memory {
      color: rgb(181, 232, 224);
    }
    #cpu {
      color: rgb(245, 194, 231);
    }
    #clock {
      color: rgb(217, 224, 238);
    }
    #custom-wall {
      color: rgb(221, 182, 242);
    }
    #temperature {
      color: rgb(150, 205, 251);
    }
    #backlight {
      color: rgb(248, 189, 150);
    }
    #pulseaudio {
      color: rgb(245, 224, 220);
    }
    #network {
      color: #abe9b3;
    }
    #network.disconnected {
      color: rgb(255, 255, 255);
    }
    #battery.charging, #baterry.full, #battery.discharging {
      color: rgb(250, 227, 176);
    }
    #battery.critical:not(.charging) {
      color: rgb(242, 143, 173);
    }
    #custom-powermenu {
      color: rgb(242, 143, 173);
    }
    #tray {
      padding-right: 8px;
      padding-left: 10px;
    }
    #mpd.paused {
      color: #414868;
      font-style: italic;
    }
    #mpd.stopped {
      background: transparent;
    }
    #mpd {
      color: #c0caf5;
    }
    #custom-cava-internal {
      font-family: "JetBrainsMono Nerd Font";
    }
  '';

  waybarConf = [{
    "layer" = "top";
    "position" = "top";
    modules-left = [ "wlr/workspaces" "temperature" "mpd" ];
    modules-center = [ "clock" ];
    modules-right = [
      "pulseaudio"
      "backlight"
      "memory"
      "cpu"
      "network"
      "custom/powermenu"
      "tray"
    ];
    "wlt/workspaces" = {
      "format" = "{icon}";
      "on-click" = "activate";
    };
    "pulseaudio" = {
      "scroll-step" = 1;
      "format" = "{icon} {volume}%";
      "format-muted" = "婢 Muted";
      "format-icons" = { "default" = [ "" "" "" ]; };
      "states" = { "warning" = 85; };
      "on-click" = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
      "tooltip" = false;
    };
    "clock" = {
      "interval" = 1;
      "format" = "{:%I:%M %p %A %b %d}";
      "tooltip" = true;
      "tooltip-format" = "<tt><small>{calendar}</small></tt>";
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
