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

      histSize = 10000;

      shellInit = ''
        export PATH=$HOME/.local/bin:$PATH
        export PATH=$HOME/.npm-global/bin:$PATH

        eval "$(zoxide init zsh)"

        alias cat="bat --theme Dracula"
        alias ls="exa --icons"
        alias l="exa --icons -lbF --git"
        alias ll="exa --icons -lbGF --git"
        alias llm="exa --icons -lbGd --git --sort=modified"
        alias la="exa --icons -lbhHigUmuSa --time-style=long-iso --git --color-scale"
        alias lx="exa --icons -lbhHigUmuSa@ --time-style=long-iso --git --color-scale"
        alias lS="exa --icons -1"
        alias lt="exa --icons --tree --level=2"
      '';
    };

    fzf = {
      keybindings = true;
      fuzzyCompletion = true;
    };
  };

  environment = { systemPackages = with pkgs; [ bat exa zoxide ]; };
}
