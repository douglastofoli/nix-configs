{
  custom-config,
  lib,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
  cfg = custom-config.lf;
in {
  options.lf = {
    enable = mkEnableOption {
      description = "Enables LF";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs.lf = {
      inherit (cfg) enable;

      commands = {
        editor-open = ''$$EDITOR $f'';
        mkdir = ''
          ''${{
            printf "Directory Name: "
            read DIR
            mkdir $DIR
          }}
        '';
      };

      keybindings = {
        "\\\"" = "";
        o = "";
        c = "mkdir";
        "." = "set hidden!";
        "`" = "mark-load";
        "\\'" = "mark-load";
        "<enter>" = "open";

        "g~" = "cd";
        gh = "cd";
        "g/" = "/";

        ee = "editor-open";
      };

      settings = {
        preview = true;
        hidden = true;
        drawbox = true;
        icons = true;
        ignorecase = true;
      };
    };

    xdg.configFile."lf/icons".source = ../../dotfiles/lf/icons;
  };
}
