# Main system configuration.

{ config, lib, pkgs, inputs, user, location, ... }:

{
  imports = (import ../modules/editors) ++ (import ../modules/shell);

  users.users = {
    root = {
      isSystemUser = true;
      shell = pkgs.zsh;
    };

    ${user} = {
      isNormalUser = true;
      extraGroups =
        [ "audio" "docker" "input" "networkmanager" "video" "wheel" ];
      shell = pkgs.zsh;
      initialPassword = "123456";
    };
  };

  security = {
    sudo.wheelNeedsPassword = true;
    rtkit.enable = true;
    polkit.enable = true;
  };

  time.timeZone = "America/Sao_Paulo";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2";
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      # pinentryFlavor = "curses";
    };
  };

  services = {
    devmon.enable = true;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      jack.enable = true;
      wireplumber.enable = false;
      media-session.enable = true;
      config.pipewire = { "default.metadata" = true; };
      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [{ "device.name" = "~bluez_card.*"; }];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              # mSBC is not expected to work on all headset + adapter combinations.
              "bluez5.msbc-support" = true;
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
          actions = { "node.pause-on-idle" = false; };
        }
      ];
    };
  };

  fonts.fonts = with pkgs; [
    carlito
    vegur
    font-awesome
    corefonts

    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  environment = {
    shells = [ pkgs.zsh ];

    variables = {
      EDITOR = "emacs";
      VISUAL = "emacs";
      BROWSER = "firefox";
      TERMINAL = "alacritty";
    };

    systemPackages = with pkgs; [
      gcc
      gnumake
      brightnessctl
      killall
      vim
      pciutils
      usbutils
      wget

      (if config.networking.networkmanager.enable then
        networkmanagerapplet
      else
        "")
    ];
  };

  nix = {
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nixVersions.unstable;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
    '';
  };

  nixpkgs.config = {
    allowUnfree = true;

    permittedInsecurePackages = [ "electron-12.2.3" "electron-13.6.9" ];
  };

  system = {
    autoUpgrade = {
      enable = false;
      allowReboot = false;
      channel = "https://nixos.org/channels/nixos-unstable";
    };
    stateVersion = "22.11";
  };
}
