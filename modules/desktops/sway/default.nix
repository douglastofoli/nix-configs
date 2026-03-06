{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkMerge types;
in {
  options.sway = {
    enable = mkEnableOption {
      description = "Enables Sway";
      type = types.bool;
      default = false;
    };

    user = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "User to install default sway config for (via home-manager). Set to enable.";
    };
  };

  config = mkIf config.sway.enable (mkMerge [
    {
      programs.sway = {
        enable = true;
        xwayland.enable = true;
        wrapperFeatures.gtk = true;
      };

      xdg.portal = {
        enable = true;
      };

      services = {
        gnome.gnome-keyring.enable = true;

        greetd = {                                                      
          enable = true;                                                         
          settings = {                                                           
            default_session = {                                                  
              command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
              user = "greeter";                                                  
            };                                                                   
          };                                                                     
        };
      };

      security.pam.services = {
        greetd.enableGnomeKeyring = true;
        swaylock.enableGnomeKeyring = true;
      };

      programs.regreet.enable = true;

      environment.systemPackages = with pkgs; [
        grim
        slurp
        swappy
        foot
        swaynotificationcenter
        libnotify
        wofi
        swaylock-effects
      ];
    }
    
    (mkIf (config.sway.user != null) {
      home-manager.users.${config.sway.user} = {
        xdg.configFile."sway" = {
          source = ../../../dotfiles/config/sway;
          recursive = true;
        };
        xdg.configFile."foot" = {
          source = ../../../dotfiles/config/foot;
          recursive = true;
        };
        xdg.configFile."swaylock" = {
          source = ../../../dotfiles/config/swaylock;
          recursive = true;
        };
        xdg.configFile."wofi" = {
          source = ../../../dotfiles/config/wofi;
          recursive = true;
        };
      };
    })
  ]);
}
