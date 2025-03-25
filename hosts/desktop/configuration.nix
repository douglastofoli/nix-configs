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
  xmonad.enable = true;
  gnome.enable = false;

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
    extraGroups = ["audio" "camera" "lp" "storage" "video" "wheel"];
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
      LC_TIME = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
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
      dnssec = "true";
      domains = ["~."];
      fallbackDns = [
        "45.90.28.0#91823f.dns.nextdns.io"
        "2a07:a8c0::#91823f.dns.nextdns.io"
        "45.90.30.0#91823f.dns.nextdns.io"
        "2a07:a8c1::#91823f.dns.nextdns.io"
      ];
      dnsovertls = "true";
    };
  };

  networking.hostName = "desktop";

  programs = {
    dconf.enable = true;
    nix-ld.enable = true;
  };

  fonts.packages = with pkgs; [
    cantarell-fonts
    corefonts # Microsoft fonts
    font-awesome
    liberation_ttf
    mononoki
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    roboto
    ubuntu_font_family
    nerd-fonts.jetbrains-mono
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
      tdesktop

      #Dev
      dbeaver-bin

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
      zen-browser
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
      zen-browser = super.zen-browser.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://github.com/zen-browser/desktop/releases/download/1.10b/zen.linux-x86_64.tar.xz";
          sha256 = "sha256:06v8caplc3qakqc9ifyfr0zmzpg83m86kc8yy8yaln77hxvw7lbz";
        };
      });
    })
  ];

  system.stateVersion = "${vars.stateVersion}";
}
