{
  flake.modules.nixos.waybar =
    { config, pkgs, ... }:
    {
      programs = {
        waybar.enable = true;
      };
    };

  flake.modules.homeManager.waybar =
    {
      config,
      host,
      lib,
      pkgs,
      ...
    }:
    let
      monitor0 = builtins.head host.monitors;
      fontFamily = "JetBrainsMono Nerd Font";
    in
    {
      wayland.windowManager.sway.systemd.enable = lib.mkDefault true;

      programs.waybar = {
        enable = true;
        systemd.enable = true;

        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 32;
            spacing = 4;
            margin-top = 0;
            margin-bottom = 0;
            output = [ monitor0.name ];

            modules-left = [
              "sway/workspaces"
              "mpris"
            ];
            modules-center = [ "sway/window" ];
            modules-right = [
              "cpu"
              "memory"
              "clock"
              "tray"
            ];

            "sway/workspaces" = {
              format = "{name}";
              persistent-workspaces = {
                "*" = 6;
              };
              cursor = true;
            };

            "mpris" = {
              format = "{player_icon} {dynamic}";
              "format-paused" = "{status_icon} {dynamic}";
              "max-length" = 42;
              "interval" = 5;
            };

            "sway/window" = {
              format = "{title}";
              "max-length" = 80;
              "rewrite" = {
                "(.*) — Mozilla Firefox" = "$1";
                "(.*) - Google Chrome" = "$1";
              };
            };

            "cpu" = {
              interval = 2;
              format = "󰻠 {usage}%";
              tooltip = true;
            };

            "memory" = {
              interval = 2;
              format = "󰍛 {percentage}%";
              tooltip-format = "{used:0.1f} GiB / {total:0.1f} GiB";
            };

            "clock" = {
              format = "󰃭 {:%a %d %b  %H:%M}";
              "format-alt" = "󰥔 {:%Y-%m-%d %H:%M:%S}";
              tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
              interval = 60;
            };

            "tray" = {
              icon-size = 20;
              spacing = 10;
              cursor = true;
            };
          };
        };

        style = ''
          * {
            border: none;
            border-radius: 0;
            font-family: ${fontFamily};
            font-size: 12px;
            min-height: 0;
          }

          window#waybar {
            opacity: 0.9;
            background-color: rgba(30, 31, 41, 230);
            color: #f8f8f2;
            border-bottom: 2px solid #282a36;
          }

          window#waybar * {
            font-family: inherit;
            font-size: inherit;
          }

          tooltip {
            font-family: ${fontFamily};
            font-size: 12px;
            background: #282a36;
            color: #f8f8f2;
            border: 1px solid #6272a4;
          }

          #workspaces {
            padding: 0 4px;
          }

          #workspaces button {
            padding: 0 6px;
            color: #6272a4;
            background: transparent;
          }

          #workspaces button:hover {
            color: #f8f8f2;
            background: #282a36;
          }

          #workspaces button.focused {
            color: #282a36;
            background: #bd93f9;
          }

          #workspaces button.urgent {
            color: #282a36;
            background: #ff5555;
          }

          #mpris {
            padding: 0 10px;
            color: #f8f8f2;
            background: #282a36;
            border-radius: 6px;
            margin: 4px 0;
          }

          #mpris.paused {
            color: #6272a4;
          }

          #window {
            padding: 0 12px;
            color: #f8f8f2;
            font-weight: 500;
          }

          #window.empty {
            color: #6272a4;
          }

          #cpu,
          #memory,
          #clock {
            padding: 0 10px;
            color: #f8f8f2;
            background: #282a36;
            border-radius: 6px;
            margin: 4px 2px;
          }

          #cpu {
            color: #8be9fd;
          }

          #memory {
            color: #50fa7b;
          }

          #clock {
            color: #f1fa8c;
          }

          #tray {
            padding: 0 8px;
            margin: 4px 0;
          }

          #tray > .passive {
            -gtk-icon-effect: dim;
          }

          #tray > .needs-attention {
            -gtk-icon-effect: highlight;
          }
        '';
      };
    };
}
