{ nix-emacs, system, pkgs, vars, ... }:

let
  emacs = nix-emacs.packages.${system}.default.override {
    config = {
      package = pkgs.emacs29;

      checkers = {
        syntax = {
          enable = true;
          childframe = true;
        };
      };

      completion = {
        company.enable = true;
        helm.enable = false;
        ivy = {
          enable = true;
          counsel = true;
          rich = true;
          swiper = true;
        };
      };

      editor = {
        evil = {
          enable = true;
          collection = true;
        };
        fold.enable = true;
        snippets.enable = true;
      };

      emacs = {
        dired = {
          enable = true;
          icons = true;
          ranger = false;
        };
      };

      ui = {
        fonts = {
          default = {
            font = "JetBrainsMono Nerd Font";
            height = 110;
            weight = "medium";
          };
          variablePitch = {
            font = "Ubuntu";
            height = 110;
            weight = "medium";
          };
          fixedPitch = {
            font = "JetBrainsMono Nerd Font";
            height = 110;
            weight = "medium";
          };
          fontLockCommentFace = "italic";
          fontLockKeywordFace = "italic";
          lineSpacing = 0.12;
        };

        emacs.themes.name = "dracula";

        ligatures.enable = true;

        nogui = false;
        menuBar = false;
        toolBar = false;
        scrollBar = false;

        ringBell = false;

        which-key = {
          enable = true;
          separator = " → ";
          sideWindowLocation = "bottom";
        };
      };
    };
  };
in {
  services.emacs = {
    enable = true;
    package = emacs;
    defaultEditor = true;
  };
}
