{
  programs = {
    starship = {
      enable = true;

      settings = {
        add_newline = false;

        character = {
          success_symbol = "[➜](bold #50fa7b) ";
          error_symbol = "[✗](bold #ff5555) ";
          vicmd_symbol = "[V](bold #bd93f9) ";
        };
      };
    };

    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;

      histSize = 5000;

      shellInit = ''
        export GPG_TTY=$(tty)

        PATH=$HOME/.emacs.d/bin:$PATH
        PATH=$HOME/.local/bin:$PATH
      '';
    };
  };
}
