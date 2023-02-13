{ config, lib, pkgs, user, ... }:

let
  waybarStyle = ''
    * {
      border: none;
      border-radius: 0;
      font-family: "JetBrainsMono Nerd Font";
      min-height: 15px;
    }
    window#waybar {
      background: transparent;
      padding: 0;
      margin: 0;
    }
    window#waybar.hidden {
      opacity: 0.2;
    }
    #window {
      margin-left: 8px;
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      color: #11111b;
      background-image: linear-gradient(to right, rgba(180, 190, 254, 1), rgba(205, 214, 244, 1), rgba(186, 194, 222, 1));
    }
    #network {
      margin-left: 8px;
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      background-image: linear-gradient(to right, rgba(127, 132, 156, 1), rgba(147, 153, 178, 1));
    }
    #disk {
      margin-left: 8px;
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      background-image: linear-gradient(to right, rgba(49, 50, 68, 1), rgba(69, 71, 90, 1));
    }
    #workspaces {
      margin-right: 8px;
      border-radius: 10px;
      transition: none;
      background: #171727;
    }
    #workspaces button {
      transition: none;
      color: #7c818c;
      background: transparent;
      padding: 5px;
      font-size: 16px;
    }
    #workspaces button.persistent {
      color: #7c818c;
      font-size: 16px;
    }
    #workspaces button:hover {
      transition: none;
      box-shadow: inherit;
      text-shadow: inherit;
      border-radius: inherit;
      color: #383c4a;
      background-image: linear-gradient(to right, rgba(148, 226, 213, 1), rgba(137, 180, 250, 1));
    }
    #workspaces button.active {
      color: white;
      font-size: 20px;
    }
    #language {
      padding-left: 16px;
      padding-right: 8px;
      border-radius: 10px 0px 0px 10px;
      transition: none;
      color: #ffffff;
      background: #383c4a;
    }
    #keyboard-state {
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px 10px 10px 10px;
      transition: none;
      color: #ffffff;
      background-image: linear-gradient(to right, rgba(17, 17, 27, 1), rgba(24, 24, 37, 1));
    }
    #keyboard-state > label.locked {
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px 10px 10px 10px;
      background-image: linear-gradient(to right, rgba(148, 226, 213, 1), rgba(137, 180, 250, 1));
      color: #11111d
    }
    #mode {
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background: #383c4a;
    }
    #clock {
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px 0px 0px 10px;
      transition: none;
      color: #11111b;
      background-image: linear-gradient(to right, rgba(243, 139, 168, 1), rgba(235, 160, 172, 1), rgba(250, 179, 135, 1));
    }
    #custom-weather {
      padding-right: 16px;
      border-radius: 0px 10px 10px 0px;
      transition: none;
      color: #11111b;
      background-image: linear-gradient(to right, rgba(250, 179, 135, 1), rgba(249, 226, 175, 1));
    }
    #pulseaudio {
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background-image: linear-gradient(to right, rgba(88, 91, 112, 1), rgba(108, 112, 134, 1));
    }
    #pulseaudio.muted {
      background-image: linear-gradient(to right, rgba(148, 226, 213, 1), rgba(137, 180, 250, 1));
      background-color: #94e2d5;
      color: #11111b;
    }
    #custom-memory {
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background-image: linear-gradient(to right, rgba(69, 71, 90, 1), rgba(88, 91, 112, 1));
    }
    #temperature {
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background: #313244;
    }
    #temperature.critical {
      background-color: #eb4d4b;
    }
    #battery {
      margin-right: 8px;
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background-image: linear-gradient(to right, rgba(108, 112, 134, 1), rgba(127, 132, 156, 1));
    }
    #battery.charging {
      color: #ffffff;
      background-color: #26A65B;
    }
    #battery.warning:not(.charging) {
      background-color: #ffbe61;
      color: black;
    }
    #battery.critical:not(.charging) {
      background-color: #f53c3c;
      color: #ffffff;
      animation-name: blink;
      animation-duration: 0.5s;
      animation-timing-function: linear;
      animation-iteration-count: infinite;
      animation-direction: alternate;
    }
    #tray {
      padding-left: 16px;
      padding-right: 16px;
      border-radius: 10px;
      transition: none;
      color: #ffffff;
      background-image: linear-gradient(to right, rgba(148, 226, 213, 1), rgba(137, 180, 250, 1));
    }
    @keyframes blink {
      to {
        background-color: #ffffff;
        color: #313244;
      }
    }
  '';

  waybarConf = [{
    "layer" = "top";
    "height" = 15;
    "margin" = "3 5 3 5";

    "modules-left" = [ "keyboard-state" "wlr/workspaces" "hyprland/window" ];
    "modules-center" = [ "clock" "custom/weather" ];
    "modules-right" = [
      "disk"
      "custom/memory"
      "temperature"
      "pulseaudio"
      "battery"
      "network"
      "tray"
    ];

    "wlr/taskbar" = {
      "format" = "{icon}";
      "icon-size" = 12;
      "icon-theme" = "Numix-Circle";
      "tooltip-format" = "{title}";
      "on-click" = "activate";
      "on-click-middle" = "close";
      "ignore-list" = [ "Alacritty" ];
    };
    "disk" = {
      "interval" = 60;
      "format" = "{free}";
      "path" = "/";
    };
    "network" = {
      "format" = "{ifname}";
      "format-wifi" = "{essid} ({signalStrength}%)";
      "format-ethernet" = "{ipaddr}/{cidr} ";
      "format-disconnected" = "";
      "tooltip-format" = "{ifname} via {gwaddr} ";
      "tooltip-format-wifi" = "{essid} ({signalStrength}%)";
      "tooltip-format-ethernet" = "{ifname} ";
      "tooltip-format-disconnected" = "Disconnected";
      "max-length" = 60;
      "on-click" = "nm-connection-editor";
    };
    "hyprland/window" = {
      "format" = "-> {}";
      "separate-outputs" = true;
      "max-length" = 40;
    };
    "wlr/workspaces" = {
      "format" = "{icon}";
      "format-icons" = {
        "active" = "";
        "default" = "";
      };
      "active-only" = false;
      "on-click" = "activate";
    };
    "sway/language" = {
      "format" = "{} ";
      "min-length" = 5;
      "tooltip" = false;
    };
    "keyboard-state" = {
      "capslock" = true;
      "format" = "{name} {icon}";
      "format-icons" = {
        "locked" = " ";
        "unlocked" = "";
      };
    };
    "sway/mode" = { "format" = ''<span style="italic">{}</span>''; };
    "clock" = {
      "format" = "{:%a, %d %b, %H:%M %p}";
      "tooltip-format" = ''
        <big>{:%Y %B}</big>
        <tt><small>{calendar}</small></tt>
      '';
    };
    "custom/weather" = {
      "format" = "{}";
      "tooltip" = true;
      "interval" = 1800;
      "exec" = "$HOME/.local/bin/weather.py";
      "return-type" = "json";
    };
    "pulseaudio" = {
      "scrolling-step" = 5;
      "format" = "{volume}% {icon} {format_source}";
      "format-bluetooth" = "{volume}% {icon}  {format_source}";
      "format-bluetooth-muted" = " {icon}  {format_source}";
      "format-muted" = "婢 {format_source}";
      "format-source" = "{volume}% ";
      "format-source-muted" = "";
      "format-icons" = {
        "headphone" = "";
        "hands-free" = "";
        "headset" = "";
        "phone" = "";
        "portable" = "";
        "car" = "";
        "default" = [ "奄" "奔" "墳" ];
      };
      "on-click" = "pavucontrol";
      "on-click-middle" = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
      "min-length" = 13;
    };
    "custom/memory" = {
      "format" = "{} ";
      "interval" = 3;
      "exec" = "free -h | awk '/Mem:/{printf $3}'";
      "tooltip" = false;
      "on-click" = "$TERMINAL -e btop";
    };
    "temperature" = {
      "hwmon-path" =
        "/sys/devices/platform/coretemp.0/hwmon/hwmon1/temp1_input";
      "critical-threshold" = 80;
      "format-critical" = "{temperatureC}°C {icon}";
      "format" = "{temperatureC}°C {icon}";
      "format-icons" = [ "" "" "" "" "" ];
      "tooltip" = false;
    };
    "battery" = {
      "states" = {
        "warning" = 30;
        "critical" = 15;
      };
      "format" = "{capacity}% {icon}";
      "format-charging" = "{capacity}% ";
      "format-plugged" = "{capacity}% ";
      "format-alt" = "{time} {icon}";
      "format-icons" = [ "" "" "" "" "" "" "" "" "" "" ];
      "on-update" = "$HOME/.local/bin/check_battery.sh";
    };
    "tray" = {
      "icon-size" = 16;
      "spacing" = 1;
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
