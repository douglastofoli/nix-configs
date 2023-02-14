{ config, lib, pkgs, host, ... }:

let
  execute = ''
    exec-once=dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
    exec-once=${pkgs.swaybg}/bin/swaybg -m center -i $HOME/.config/wallpaper.png
    exec-once=${pkgs.waybar}/bin/waybar
    exec-once=${pkgs.blueman}/bin/blueman-applet
    exec-once=${pkgs.networkmanagerapplet}/bin/nm-applet --indicator
  '';
in let
  hyprlandConf = with host; ''
    monitor = ,preferred,auto,auto

    input {
        kb_layout = br,us
        kb_variant =
        kb_model =
        kb_options = grp:win_space_toggle
        kb_rules =

        follow_mouse = 1

        touchpad {
            natural_scroll = yes
        }

        sensitivity = 0.7
    }

    general {
        gaps_in = 3
        gaps_out = 5
        border_size = 3
        col.active_border = rgba(f38ba8FF) rgba(cba6f7FF) rgba(89b4faFF) rgba(fab387FF) 45deg
        col.inactive_border = rgba(59595900)
        layout = dwindle
    }

    decoration {
        rounding = 8
        blur = yes
        blur_size = 6.8
        blur_passes = 3
        blur_new_optimizations = on
        inactive_opacity = 0.98

        drop_shadow = no
        shadow_range = 4
        shadow_render_power = 3
        col.shadow = rgba(1a1a1aee)
    }

    animations {
        enabled = yes

        bezier = myBezier, 0.05, 0.9, 0.1, 1.05
        bezier = overshot, 0.13, 0.99, 0.29, 1.1

        animation = windows, 1, 5, overshot, popin
        animation = border, 1, 5, default
        animation = fade, 1, 5, default
        animation = workspaces, 1, 6, default
    }

    dwindle {
        pseudotile = yes
        pseudotile = true
        preserve_split = yes
        force_split = 2
        no_gaps_when_only = 1
    }

    master {
        new_is_master = true
        new_on_top = true,
        no_gaps_when_only = true
    }

    gestures {
        workspace_swipe = on
        workspace_swipe_min_speed_to_force = 50
        workspace_swipe_distance = 500
    }

    misc {
       disable_hyprland_logo = on
       enable_swallow = true
       no_vfr = true
       animate_manual_resizes = false
    }

    device:epic mouse V1 {
        sensitivity = -0.5
    }

    $mainMod = SUPER

    bind = $mainMod, Return, exec, $TERMINAL
    bind = $mainMod SHIFT, Q, killactive,
    bind = $mainMod SHIFT, E, exit,
    bind = $mainMod, V, togglefloating,
    bind = $mainMod, D, exec, wofi --show drun
    bind = $mainMod, P, pseudo,
    bind = $mainMod, J, togglesplit,

    bind = $mainMod, left, movefocus, l
    bind = $mainMod, right, movefocus, r
    bind = $mainMod, up, movefocus, u
    bind = $mainMod, down, movefocus, d

    bind = SUPER_SHIFT, left, movewindow, l
    bind = SUPER_SHIFT, right, movewindow, r
    bind = SUPER_SHIFT, up, movewindow, u
    bind = SUPER_SHIFT, down, movewindow, d

    bind = $mainMod, 1, workspace, 1
    bind = $mainMod, 2, workspace, 2
    bind = $mainMod, 3, workspace, 3
    bind = $mainMod, 4, workspace, 4
    bind = $mainMod, 5, workspace, 5
    bind = $mainMod, 6, workspace, 6
    bind = $mainMod, 7, workspace, 7
    bind = $mainMod, 8, workspace, 8
    bind = $mainMod, 9, workspace, 9
    bind = $mainMod, 0, workspace, 10

    # Move window, doesnt switch to the workspace
    bind = $mainMod SHIFT, 1, movetoworkspacesilent, 1
    bind = $mainMod SHIFT, 2, movetoworkspacesilent, 2
    bind = $mainMod SHIFT, 3, movetoworkspacesilent, 3
    bind = $mainMod SHIFT, 4, movetoworkspacesilent, 4
    bind = $mainMod SHIFT, 5, movetoworkspacesilent, 5
    bind = $mainMod SHIFT, 6, movetoworkspacesilent, 6
    bind = $mainMod SHIFT, 7, movetoworkspacesilent, 7
    bind = $mainMod SHIFT, 8, movetoworkspacesilent, 8
    bind = $mainMod SHIFT, 9, movetoworkspacesilent, 9
    bind = $mainMod SHIFT, 0, movetoworkspacesilent, 10

    # Move window, switch to the workspace
    bind = $mainMod CTRL, 1, movetoworkspace, 1
    bind = $mainMod CTRL, 2, movetoworkspace, 2
    bind = $mainMod CTRL, 3, movetoworkspace, 3
    bind = $mainMod CTRL, 4, movetoworkspace, 4
    bind = $mainMod CTRL, 5, movetoworkspace, 5
    bind = $mainMod CTRL, 6, movetoworkspace, 6
    bind = $mainMod CTRL, 7, movetoworkspace, 7
    bind = $mainMod CTRL, 8, movetoworkspace, 8
    bind = $mainMod CTRL, 9, movetoworkspace, 9
    bind = $mainMod CTRL, 0, movetoworkspace, 10

    bind = $mainMod, mouse_down, workspace, e+1
    bind = $mainMod, mouse_up, workspace, e-1

    bindm = $mainMod, mouse:272, movewindow
    bindm = $mainMod, mouse:273, resizewindow

    # Volume, brightness, media player
    bind = , XF86AudioRaiseVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ +5%
    bind = , XF86AudioLowerVolume, exec, pactl set-sink-volume @DEFAULT_SINK@ -5%
    bind = , XF86AudioMute, exec, pactl set-sink-mute @DEFAULT_SINK@ toggle
    bind = , XF86MonBrightnessUp, exec, light -A 5
    bind = , XF86MonBrightnessDown, exec, light -U 5
    bind = , XF86AudioPlay, exec, playerctl play-pause
    bind = , XF86AudioNext, exec, playerctl next
    bind = , XF86AudioPrev, exec, playerctl previous

    # Scratchpad
    bind = $mainMod, minus, movetoworkspace,special
    bind = $mainMod, equal, togglespecialworkspace

    # Screenshot
    bind = , print, exec, ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.swappy}/bin/swappy -f - -o ~/Pictures/Screenshots/$(date +%Hh_%Mm_%Ss_%d_%B_%Y).png && notify-send "Saved to ~/Pictures/$(date +%Hh_%Mm_%Ss_%d_%B_%Y).png"

    # Auto start
    ${execute}

    # Window rules
    windowrule = float, title: ^(Picture-in-Picture)$
    windowrule = pin, title: ^(Picture-in-Picture)$
    windowrule = move 75% 75%, title: ^(Picture-in-Picture)$
    windowrule = size 24% 24%, title: ^(Picture-in-Picture)$

    windowrule = workspace 1, title: ^(Mozilla Firefox)$
    windowrule = workspace 4, title: ^(Telegram)$
    windowrule = workspace 5, title: ^(Discord)$

    windowrule = float, title: ^(Telegram)$
    windowrule = move 75% 75%, title: ^(Telegram)$
    windowrule = size 24% 24%, title: ^(Telegram)$

    windowrule = float, title: ^(Bluetooth Devices)$
    windowrule = size 1000 600, title: ^(Bluetooth Devices)$
    windowrule = center, title: ^(Bluetooth Devices)$
  '';
in {
  xdg.configFile."hypr/hyprland.conf".text = hyprlandConf;

  programs.swaylock.settings = {
    color = "000000f0";
    font-size = "24";
    indicator-idle-visible = false;
    indicator-radius = 100;
    indicator-thickness = 20;
    inside-color = "00000000";
    inside-clear-color = "00000000";
    inside-ver-color = "00000000";
    inside-wrong-color = "00000000";
    key-hl-color = "79b360";
    line-color = "000000f0";
    line-clear-color = "000000f0";
    line-ver-color = "000000f0";
    line-wrong-color = "000000f0";
    ring-color = "ffffff50";
    ring-clear-color = "bbbbbb50";
    ring-ver-color = "bbbbbb50";
    ring-wrong-color = "b3606050";
    text-color = "ffffff";
    text-ver-color = "ffffff";
    text-wrong-color = "ffffff";
    show-failed-attempts = true;
  };
}
