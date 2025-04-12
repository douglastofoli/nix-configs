{pkgs, ...}: let
  execute = ''
    exec-once=dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    exec-once=systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP

    exec-once=${pkgs.swaybg}/bin/swaybg -m fill -i $HOME/.config/wallpaper.jpg
    exec-once=${pkgs.waybar}/bin/waybar
    exec-once=${pkgs.blueman}/bin/blueman-applet

    exec-once=$HOME/.config/hypr/scripts/sleep.sh

    exec=${pkgs.tdesktop}/bin/telegram-desktop
    exec=${pkgs.zen-browser}/bin/zen
  '';
in let
  hyprlandConfig = ''
    # Monitors
    monitor=,preferred,auto,auto

    # Programs
    $browser = zen
    $terminal = alacritty
    $fileManager = pcmanfm
    $menu = wofi --show drun

    # Autostart
    ${execute}

    # Environment Variables
    env = XCURSOR_SIZE,24
    env = HYPRCURSOR_SIZE,24

    # Look and feel
    general {
      gaps_in = 5
      gaps_out = 20

      border_size = 2

      col.active_border = rgba(33ccffee) rgba(00ff99ee) 45deg
      col.inactive_border = rgba(595959aa)

      resize_on_border = false

      allow_tearing = false

      layout = dwindle
    }

    decoration {
      rounding = 10
      rounding_power = 2

      active_opacity = 1.0
      inactive_opacity = 1.0

      shadow {
        enabled = true
        range = 4
        render_power = 3
        color = rgba(1a1a1aee)
      }

      blur {
        enabled = true
        size = 3
        passes = 1

        vibrancy = 0.1696
      }
    }

    animations {
      enabled = yes, please :)

      bezier = easeOutQuint,0.23,1,0.32,1
      bezier = easeInOutCubic,0.65,0.05,0.36,1
      bezier = linear,0,0,1,1
      bezier = almostLinear,0.5,0.5,0.75,1.0
      bezier = quick,0.15,0,0.1,1

      animation = global, 1, 10, default
      animation = border, 1, 5.39, easeOutQuint
      animation = windows, 1, 4.79, easeOutQuint
      animation = windowsIn, 1, 4.1, easeOutQuint, popin 87%
      animation = windowsOut, 1, 1.49, linear, popin 87%
      animation = fadeIn, 1, 1.73, almostLinear
      animation = fadeOut, 1, 1.46, almostLinear
      animation = fade, 1, 3.03, quick
      animation = layers, 1, 3.81, easeOutQuint
      animation = layersIn, 1, 4, easeOutQuint, fade
      animation = layersOut, 1, 1.5, linear, fade
      animation = fadeLayersIn, 1, 1.79, almostLinear
      animation = fadeLayersOut, 1, 1.39, almostLinear
      animation = workspaces, 1, 1.94, almostLinear, fade
      animation = workspacesIn, 1, 1.21, almostLinear, fade
      animation = workspacesOut, 1, 1.94, almostLinear, fade
    }

    $mainMod=SUPER

    input {
      kb_layout=br,us
      kb_options=grp:win_space_toggle

      follow_mouse=1
      float_switch_override_focus=true

      sensitivity=0.7
    }


    decoration {
      rounding=8
      inactive_opacity=0.95
    }


    gestures {
      workspace_swipe=on
      workspace_swipe_fingers=3
      workspace_swipe_distance=500
    }

    misc {
      disable_hyprland_logo=on
      enable_swallow=true
    }

    bind=$mainMod,Return,exec,$TERMINAL
    bind=$mainMod,D,exec,wofi --show drun
    bind=$mainMod,L,exec,~/.config/hypr/scripts/lock.sh

    # Auto start
  '';
in {
  xdg.configFile."hypr/hyprland.conf".text = hyprlandConfig;
  xdg.configFile."hypr/scripts".source = ./scripts;
}
