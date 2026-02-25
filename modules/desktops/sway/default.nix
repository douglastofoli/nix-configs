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
      };

      xdg.portal = {
        enable = true;
      };

      # Login: greetd + regreet (ReGreet runs in Cage, lists Wayland sessions including Hyprland)
      services.greetd = {                                                      
        enable = true;                                                         
        settings = {                                                           
          default_session = {                                                  
          command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
          user = "greeter";                                                  
          };                                                                   
        };                                                                     
      };
      security.pam.services = {
        greetd.enableGnomeKeyring = true;
        swaylock.enableGnomeKeyring = true;
      };

      programs.regreet.enable = true;

      # Default Hyprland config (example) + packages referenced by it
      environment.systemPackages = with pkgs; [
     
      ];
    }
    
    (mkIf (config.hyprland.user != null) {
      home-manager.users.${config.hyprland.user} = {
        xdg.configFile."sway" = {
            source = ./config;
        };
      };
    })
  ]);
}
