# Main system configuration.

{ config, lib, pkgs, inputs, user, location, ... }:

{
  imports =
    [ (import ./activation.nix) ]; # ! Comment this out on first install !

  users.users = {
    root = {
      isSystemUser = true;
      shell = pkgs.zsh;
    };

    ${user} = {
      isNormalUser = true;
      extraGroups = [ "audio" "docker" "networkmanager" "video" "wheel" ];
      shell = pkgs.zsh;
      initialPassword = "123456";
    };
  };

  environment.etc = {
    "resolv.conf".text = ''
      nameserver 1.1.1.1
      nameserver 1.0.0.1
    '';
  };

  time.timeZone = "America/Sao_Paulo";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "en_US/ISO-8859-1"
      "pt_BR.UTF-8/UTF-8"
      "pt_BR/ISO-8859-1"
    ];
    extraLocaleSettings = {
      LC_TIME = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2";
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };
  };

  services = {
    devmon.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;

      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [{ "device.name" = "~bluez_card.*"; }];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              # mSBC is not expected to work on all headset + adapter combinations.
              "bluez5.msbc-support" = true;
              # SBC-XQ is not expected to work on all headset + adapter combinations.
              "bluez5.sbc-xq-support" = true;
            };
          };
        }
        {
          matches = [
            # Matches all sources
            {
              "node.name" = "~bluez_input.*";
            }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
        }
      ];
    };
  };

  fonts.fonts = with pkgs; [
    cantarell-fonts
    corefonts
    font-awesome
    liberation_ttf
    mononoki
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    ubuntu_font_family
    vegur

    (nerdfonts.override { fonts = [ "JetBrainsMono" "RobotoMono" ]; })
  ];

  environment = {
    shells = [ pkgs.zsh ];

    variables = {
      EDITOR = "emacs";
      TERMINAL = "alacritty";
      VISUAL = "emacs";
    };
    systemPackages = with pkgs; [
      cmake
      curl
      direnv
      gcc
      gnumake
      killall
      libnotify
      libtool
      libsecret
      nano
      scrot
      vim
      wget
      xclip

      (aspellWithDicts
        (dicts: with dicts; [ en en-computers en-science pt_BR ]))
    ];
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-wlr ];
  };

  nix = {
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nixFlakes;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
    '';
  };

  system = {
    autoUpgrade = {
      enable = false;
      allowReboot = false;
      channel = "https://nixos.org/channels/nixos-unstable";
    };
    stateVersion = "22.05";
  };
}
