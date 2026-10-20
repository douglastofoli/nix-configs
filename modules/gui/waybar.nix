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

      services.playerctld.enable = lib.mkDefault true;

      programs.waybar = {
        enable = true;
        systemd.enable = true;

        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 36;
            spacing = 8;
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
              persistent-workspaces = lib.genAttrs (map toString (lib.range 1 6)) (_: [
                monitor0.name
              ]);
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
            background-color: rgba(16, 17, 23, 0.94);
            color: #e8eaf0;
            border-bottom: 1px solid rgba(98, 114, 164, 0.35);
            box-shadow: 0 4px 24px rgba(0, 0, 0, 0.35);
          }

          window#waybar * {
            font-family: inherit;
            font-size: inherit;
          }

          tooltip {
            font-family: ${fontFamily};
            font-size: 11.5px;
            background: rgba(30, 32, 42, 0.98);
            color: #f0f1f5;
            border: 1px solid rgba(139, 233, 253, 0.25);
            border-radius: 4px;
            padding: 8px 12px;
            box-shadow: 0 8px 32px rgba(0, 0, 0, 0.45);
          }

          #workspaces {
            margin: 0;
            padding: 0;
          }

          #workspaces button {
            min-width: 0;
            padding: 0 10px;
            margin: 0;
            color: #7a7f98;
            background: transparent;
            border-radius: 0;
            border: none;
            border-bottom: 2px solid transparent;
            transition:
              color 140ms ease,
              background 140ms ease,
              border-bottom-color 140ms ease;
          }

          #workspaces button:hover {
            color: #cdd0da;
            background: rgba(255, 255, 255, 0.06);
          }

          #workspaces button.focused {
            color: #e8eaf0;
            background: rgba(189, 147, 249, 0.14);
            border-bottom-color: #bd93f9;
            font-weight: 600;
          }

          #workspaces button.urgent {
            color: #ffdfd9;
            background: rgba(255, 85, 85, 0.22);
            border-bottom-color: #ff5555;
          }

          #mpris {
            padding: 0 12px;
            margin: 0;
            color: #e8eaf0;
            background: rgba(40, 42, 54, 0.75);
            border: none;
            border-left: 1px solid rgba(255, 255, 255, 0.08);
            border-radius: 0;
            box-shadow: none;
          }

          #mpris.paused {
            color: #7a7f98;
          }

          #window {
            padding: 0 14px;
            margin: 0;
            color: #c8ccd6;
            font-weight: 500;
            background: transparent;
            border-radius: 0;
            border: none;
          }

          #window.empty {
            color: #5c6177;
          }

          #cpu,
          #memory,
          #clock {
            padding: 0 10px;
            margin: 0;
            min-width: 0;
            color: #e8eaf0;
            background: rgba(40, 42, 54, 0.75);
            border: none;
            border-left: 1px solid rgba(255, 255, 255, 0.08);
            border-radius: 0;
            box-shadow: none;
            transition: color 140ms ease;
          }

          #cpu {
            color: #9ee7ff;
            border-color: rgba(139, 233, 253, 0.22);
          }

          #memory {
            color: #73f0a0;
            border-color: rgba(80, 250, 123, 0.22);
          }

          #clock {
            color: #f5f7a8;
            font-weight: 600;
            letter-spacing: 0.02em;
          }

          #tray {
            padding: 0 8px;
            margin: 0;
            background: rgba(40, 42, 54, 0.75);
            border-radius: 0;
            border: none;
            border-left: 1px solid rgba(255, 255, 255, 0.08);
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
