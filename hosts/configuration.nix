# Main system configuration.

{ config, lib, pkgs, inputs, user, location, ... }:

{
  imports = [ (import ../modules/services/yubikey.nix) ]
    ++ (import ../modules/shell);

  #age.identityPaths = [ "/home/${user}/.ssh/id_ed25519" ];

  #age.secrets = { userpassword.file = ../secrets/userpassword.age; };
  #programs.zsh.enable = true;
  
  users.users = {
    root = {
      isSystemUser = true;
      shell = pkgs.zsh;
    };

    ${user} = {
      isNormalUser = true;
      extraGroups =
        [ "audio" "camera" "docker" "kvm" "networkmanager" "video" "wheel" ];
      shell = pkgs.zsh;
      initialPassword = "123456";      
#passwordFile = config.age.secrets.userpassword.path;
    };
  };

  time.timeZone = "America/Sao_Paulo";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_BR.UTF-8";
      LC_IDENTIFICATION = "pt_BR.UTF-8";
      LC_MEASUREMENT = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
      LC_NAME = "pt_BR.UTF-8";
      LC_NUMERIC = "pt_BR.UTF-8";
      LC_PAPER = "pt_BR.UTF-8";
      LC_TELEPHONE = "pt_BR.UTF-8";
      LC_TIME = "pt_BR.UTF-8";
    };
  };

  security = {
    rtkit.enable = true;
    polkit.enable = true;
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "br-abnt2";
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
      jack.enable = true;
    };
  };

  fonts = {
    fonts = with pkgs; [
      corefonts # Microsoft fonts
      font-awesome

      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf

      mononoki
      ubuntu_font_family

      (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    ];
  };

  environment = {
    shells = [ pkgs.zsh ];

    variables = {
      EDITOR = "emacs";
      VISUAL = "emacs";
      BROWSER = "firefox";
      TERMINAL = "alacritty";
    };

    systemPackages = with pkgs; [
      cmake
      gcc
      gnumake
      killall
      vim
      pciutils
      usbutils
      wget

      inputs.agenix.packages.x86_64-linux.default
    ];
  };

  nix = {
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 3d";
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
    allowBroken = true;
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
