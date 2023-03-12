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
        [ "audio" "camera" "docker" "networkmanager" "video" "wheel" ];
      shell = pkgs.zsh;
      initialPassword = "123456";
    };
  };

  time.timeZone = "America/Sao_Paulo";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "pt_BR.UTF-8";
      LC_MONETARY = "pt_BR.UTF-8";
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

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
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
    };
  };

  fonts = {
    fonts = with pkgs; [
      font-awesome
      corefonts

      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      liberation_ttf

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

    systemPackages = with pkgs; [ cmake gcc gnumake killall vim pciutils usbutils wget ];
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
    permittedInsecurePackages = [ "electron-12.2.3" "electron-13.6.9" ];
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
