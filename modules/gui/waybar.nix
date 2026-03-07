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
      pkgs,
      ...
    }:
    let
      monitor0 = builtins.head host.monitors;
    in
    {
      programs.waybar = {
        enable = true;

        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 30;

            modules-left = [ "sway/workspaces" ];
            modules-center = [ "sway/window" ];
            modules-right = [
              "clock"
              "tray"
            ];

            "sway/workspaces" = {
              format = "{icon}";
              persistent-workspaces = {
                "*" = 6;
              };
              cursor = true;
              format-icons = {
                active = "󰮯";
                persistent = "";
                empty = "";
              };
            };

            "sway/window" = {
              format = "{title}";
              max-length = 50;
            };

            "clock" = {
              format-time = "󰥔 {:%H:%M}";
              tooltip-format = "󰥔 {:%Y-%m-%d %H:%M}";
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
            font-family: Source Code Pro;
          }
          window#waybar {
            background: #16191C;
            color: #AAB2BF;
          }
          #workspaces button {
            padding: 0 5px;
          }
        '';
      };
    };
}
