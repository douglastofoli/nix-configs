{ inputs, ... }@flake:
{
  flake.modules.nixos.sway =
    { config, pkgs, ... }:
    {
      programs.sway = {
        enable = true;
        xwayland.enable = true;
        wrapperFeatures.gtk = true;
      };

      services.greetd = {
        enable = true;
        settings = {
          default_session = {
            command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
            user = "greeter";
          };
        };
      };
    };

  flake.modules.homeManager.sway =
    {
      config,
      host,
      lib,
      pkgs,
      ...
    }:
    let
      alt = "Mod1";
      mod = "Mod4";

      left = "h";
      down = "j";
      up = "k";
      right = "l";

      term = "${pkgs.foot}/bin/foot";
      menu = "wmenu-run -N '#282A36' -n '#F8F8F2' -S '#6272A4' -s '#F8F8F2' -f 'UbuntuMono Nerd Font 12' -p 'run:'";

      monitor = builtins.head host.monitors;
    in
    {
      wayland.windowManager.sway = {
        enable = true;
        xwayland = true;
        checkConfig = false;

        config = {
          modifier = mod;

          startup = [
            { command = "firefox"; }
          ];

          output = {
            "${monitor.name}" = {
              bg = "~/Pictures/wallpaper.jpg fill";
            };
          };

          input = {
            "type:keyboard" = {
              xkb_layout = "br,us";
              xkb_variant = "abnt2";
            };
          };

          assigns = {
            "2" = [ { class = "^Firefox$"; } ];
          };

          bars = [ ];

          gaps = {
            inner = 15;
            outer = 10;
            smartGaps = "off";
            smartBorders = "off";
          };

          colors = lib.mkForce {
            focused = {
              border = "#6272A4";
              background = "#6272A4";
              text = "#F8F8F2";
              indicator = "#6272A4";
              childBorder = "#6272A4";
            };
            focusedInactive = {
              border = "#44475A";
              background = "#44475A";
              text = "#F8F8F2";
              indicator = "#44475A";
              childBorder = "#44475A";
            };
            unfocused = {
              border = "#282A36";
              background = "#282A36";
              text = "#BFBFBF";
              indicator = "#282A36";
              childBorder = "#282A36";
            };
            urgent = {
              border = "#44475A";
              background = "#FF5555";
              text = "#F8F8F2";
              indicator = "#FF5555";
              childBorder = "#FF5555";
            };
            placeholder = {
              border = "#282A36";
              background = "#282A36";
              text = "#F8F8F2";
              indicator = "#282A36";
              childBorder = "#282A36";
            };
            background = "#F8F8F2";
          };

          keybindings = {
            "${mod}+Return" = "exec ${term}";
            "${mod}+Shift+q" = "kill";
            "${mod}+d" = "exec ${menu}";
            "${mod}+Shift+c" = "reload";
            "${mod}+Shift+e" =
              "exec ${pkgs.sway}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' '${pkgs.sway}/bin/swaymsg exit'";

            # Move your focus around
            "${mod}+${left}" = "focus left";
            "${mod}+${down}" = "focus down";
            "${mod}+${up}" = "focus up";
            "${mod}+${right}" = "focus right";
            # Or use $mod+[up|down|left|right]
            "${mod}+Left" = "focus left";
            "${mod}+Down" = "focus down";
            "${mod}+Up" = "focus up";
            "${mod}+Right" = "focus right";

            # Move the focused window with the same, but add Shift
            "${mod}+Shift+${left}" = "move left";
            "${mod}+Shift+${down}" = "move down";
            "${mod}+Shift+${up}" = "move up";
            "${mod}+Shift+${right}" = "move right";
            # Ditto, with arrow keys
            "${mod}+Shift+Left" = "move left";
            "${mod}+Shift+Down" = "move down";
            "${mod}+Shift+Up" = "move up";
            "${mod}+Shift+Right" = "move right";

            # Workspaces
            # Switch to workspace
            "${mod}+1" = "workspace number 1";
            "${mod}+2" = "workspace number 2";
            "${mod}+3" = "workspace number 3";
            "${mod}+4" = "workspace number 4";
            "${mod}+5" = "workspace number 5";
            "${mod}+6" = "workspace number 6";
            "${mod}+7" = "workspace number 7";
            "${mod}+8" = "workspace number 8";
            "${mod}+9" = "workspace number 9";
            "${mod}+0" = "workspace number 10";
            # Move focused container to workspace
            "${mod}+Shift+1" = "move container to workspace number 1";
            "${mod}+Shift+2" = "move container to workspace number 2";
            "${mod}+Shift+3" = "move container to workspace number 3";
            "${mod}+Shift+4" = "move container to workspace number 4";
            "${mod}+Shift+5" = "move container to workspace number 5";
            "${mod}+Shift+6" = "move container to workspace number 6";
            "${mod}+Shift+7" = "move container to workspace number 7";
            "${mod}+Shift+8" = "move container to workspace number 8";
            "${mod}+Shift+9" = "move container to workspace number 9";
            "${mod}+Shift+0" = "move container to workspace number 10";

            # Layout stuff
            "${mod}+b" = "splith";
            "${mod}+v" = "splitv";
            "${mod}+s" = "layout stacking";
            "${mod}+w" = "layout tabbed";
            "${mod}+e" = "layout toggle split";
            "${mod}+f" = "fullscreen";
            "${mod}+Shift+space" = "floating toggle";
            "${mod}+space" = "focus mode_toggle";
            "${mod}+a" = "focus parent";

            # Scratchpad
            "${mod}+Shift+minus" = "move scratchpad";
            "${mod}+minus" = "scratchpad show";

            # Behavior
            "${alt}+space" = "input * xkb_switch_layout next";
            "${alt}+l" = "exec swaylock";

            # Resizing containers

            # Utilities
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioMicMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";

            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +5%";

            "Print" =
              "exec sh -c 'GEOM=\"$(${pkgs.slurp}/bin/slurp)\" || exit 0; [ -n \"$GEOM\" ] || exit 0; OUT=\"$HOME/Pictures/Screenshots/screenshot-$(date +%Y-%m-%d_%H-%M-%S).png\"; ${pkgs.grim}/bin/grim -g \"$GEOM\" - | ${pkgs.swappy}/bin/swappy -f - -o \"$OUT\"'";
          };
        };

        extraConfig = ''
          default_border pixel 3
          default_floating_border pixel 3

          exec ${pkgs.blueman}/bin/blueman-applet

          for_window [app_id="org.telegram.desktop"] floating enable
        '';
      };
    };
}
