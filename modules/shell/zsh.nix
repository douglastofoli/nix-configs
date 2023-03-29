{ config, pkgs, ... }:

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

      enableCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
	
      ohMyZsh = {
        enable = true;
        plugins = [ "git" ];
      };

      histSize = 5000; 

      shellInit = ''
        export PATH=$HOME/.config/emacs/bin:$PATH
      '';
    };
  };
}
