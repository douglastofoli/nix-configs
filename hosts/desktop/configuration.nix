# Specific system configuration settings for desktop
{
  config,
  inputs,
  pkgs,
  vars,
  ...
}: {
  imports =
    [./hardware-configuration.nix]
    ++ import ../../modules/desktops
    ++ import ../../modules/programs
    ++ import ../../modules/services
    ++ import ../../modules/shells;

  docker.enable = true;
  xmonad.enable = false;
  hyprland.enable = true;
  hyprland.user = vars.user;
  sway.enable = true;
  sway.user = vars.user;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        efiSupport = true;
        devices = ["nodev"];
        useOSProber = true;
      };

      timeout = 3;
    };
  };

  users.users.${vars.user} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    initialPassword = "123456";
    extraGroups = ["audio" "camera" "lp" "storage" "video" "wheel" "networkmanager"];
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2";
  };

  time.timeZone = "${vars.timezone}";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "pt_BR.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LC_ALL = "pt_BR.UTF-8";
    };
  };

  security = {
    rtkit.enable = true;
    polkit.enable = true;
  };

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
          Experimental = true;
        };
      };
    };
  };

  services = {
    postgresql = {
      enable = true;
      extensions = with pkgs.postgresql16Packages; [
        postgis
      ];
    };

    flatpak.enable = true;

    blueman.enable = true;
    devmon.enable = true;
    gvfs.enable = true;
    udisks2.enable = true;
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
    resolved = {
      enable = true;
      settings = {
        Resolve = {
          DNSSEC = "true";
          DNSOverTLS = "true";
          Domains = ["~."];
          FallbackDNS = [
            "45.90.28.0#91823f.dns.nextdns.io"
            "2a07:a8c0::#91823f.dns.nextdns.io"
            "45.90.30.0#91823f.dns.nextdns.io"
            "2a07:a8c1::#91823f.dns.nextdns.io"
          ];
        };
      };
    };
  };

  networking.hostName = "desktop";
  networking.firewall.enable = true;
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
    ];
  };

  programs = {
    openvpn3.enable = true;
    dconf.enable = true;
    nix-ld.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };

  fonts.packages = with pkgs; [
    cantarell-fonts
    corefonts # Microsoft fonts
    font-awesome
    liberation_ttf
    mononoki
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    roboto
    ubuntu-classic
    nerd-fonts.jetbrains-mono
    nerd-fonts.caskaydia-cove
  ];

  environment = {
    shells = [pkgs.zsh];

    variables = {
      BROWSER = "${vars.browser}";
      EDITOR = "${vars.editor}";
      TERMINAL = "${vars.terminal}";
      TZ = "${config.time.timeZone}";
      VISUAL = "${vars.editor}";
    };

    systemPackages = with pkgs; [
      # Apps
      gimp
      google-chrome
      insomnia
      obs-studio
      spotify
      telegram-desktop

      networkmanagerapplet

      #Dev
      dbeaver-bin
      docker-compose

      # Files
      fd
      file-roller
      pcmanfm
      ripgrep
      unrar
      unzip
      wget
      zip

      # GNU utilities
      cmake
      gcc
      gnumake
      libtool

      # Terminal
      btop
      killall
      pciutils
      usbutils
      xdg-utils

      # Video/Audio
      pavucontrol
      vlc

      discord

      code-cursor
    ];
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 60;
  };

  nix = {
    settings = {
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nixVersions.latest;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  nixpkgs.overlays = [
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "0qzdvyyialvpiwi9mppbqvf2rvz1ps25mmygqqck0z9i2q01c1zd";
        };
      });
    })
    (self: super: {
      code-cursor = super.appimageTools.wrapType2 {
        pname = "cursor";
        version = "2.5.20";

        src = super.fetchurl {
          url = "https://downloads.cursor.com/production/511523af765daeb1fa69500ab0df5b6524424612/linux/x64/Cursor-2.5.20-x86_64.AppImage";
          sha256 = "sha256-csvj0THOX/RPi7Docv033mi8DlEOEdnjFIQ8jL7HPO8=";
        };
      };
    })
  ];

  system.stateVersion = "${vars.stateVersion}";
}
