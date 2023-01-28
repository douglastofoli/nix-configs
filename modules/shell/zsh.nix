{ pkgs, ... }:

{
  programs = {
    starship = {
      enable = true;

      settings = {
        add_newline = false;

        character = {
          success_symbol = "[➜](bold #50fa7b)";
          cancel_symbol = "[➜](bold #ffb86c)";
          error_symbol = "[➜](bold #ff5555)";
        };
      };
    };

    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;

      histSize = 5000;

      shellAliases = {
        cat = "bat -pp --theme='Nord'";
        emacs = "emacsclient -c -a 'emacs'";
        ls = "exa -G --color auto --icons -s type";
        ll = "exa -l --color always --icons -s type";
      };

      shellInit = ''
        export GPG_TTY=$(tty)

        PATH=$HOME/.emacs.d/bin:$PATH
      '';
    };
  };
}
