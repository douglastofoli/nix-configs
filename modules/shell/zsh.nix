{ lib, pkgs, ... }:

{
  programs = {
    starship = {
      enable = true;

      settings = {
        add_newline = true;

        character = {
          success_symbol = "[➜](bold #a6da95) ";
          error_symbol = "[✗](bold #ed8796) ";
          vicmd_symbol = "[V](bold #c6a0f6) ";
        };
      };
    };

    zsh = {
      enable = true;

      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
      };

      enableCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;

      histSize = 5000;

      shellInit = ''
        export GPG_TTY=$(tty)
      '';
    };

    fzf.fuzzyCompletion = true;
  };
}
