{
  config,
  lib,
  pkgs,
  inputs,
  vars,
  ...
}: {
  imports =
    import ../modules/desktops
    ++ import ../modules/editors
    ++ import ../modules/hardware
    ++ import ../modules/programs
    ++ import ../modules/services
    ++ import ../modules/shells
    ++ import ../modules/themes;

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

  security = {
    rtkit.enable = true;
    polkit.enable = true;
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
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
    };
  };

  fonts = {
    packages = with pkgs; [
      cantarell-fonts
      corefonts # Microsoft fonts
      font-awesome
      liberation_ttf
      mononoki
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto
      ubuntu_font_family

      ultimate-oldschool-pc-font-pack

      (nerdfonts.override {fonts = ["JetBrainsMono"];})
    ];
  };

  environment = {
    shells = [pkgs.zsh];

    variables = {
      TERMINAL = "${vars.terminal}";
      EDITOR = "${vars.editor}";
      VISUAL = "${vars.editor}";
      TZ = "${config.time.timeZone}"; # Fix the timezone on firefox
    };

    systemPackages = with pkgs; [
      # Apps
      google-chrome
      obs-studio

      # File Management
      rsync
      unzip
      unrar
      wget
      zip

      # GNU Utilities
      cmake
      gcc
      gnumake
      libtool

      # Terminal
      btop
      killall

      # Video/Audio
      pavucontrol
      vlc
    ];
  };

  programs = {dconf.enable = true;};

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
    package = pkgs.nixVersions.unstable;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  # nixpkgs.config.allowUnfree = true;

  system.stateVersion = "23.05";

  home-manager.users.${vars.user} = {
    home.stateVersion = "23.05";
    programs.home-manager.enable = true;
  };
}
