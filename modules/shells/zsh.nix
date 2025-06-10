{pkgs, ...}: {
  programs = {
    starship = {
      enable = true;

      settings = {
        add_newline = false;

        character = {
          success_symbol = "[➜](bold #a6da95) ";
          error_symbol = "[✗](bold #ed8796) ";
          vicmd_symbol = "[V](bold #c6a0f6) ";
        };

        gcloud = {detect_env_vars = ["GCLOUD_HOME"];};
      };
    };

    zsh = {
      enable = true;

      enableCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;

      ohMyZsh = {
        enable = true;
        plugins = ["git" "tmux"];
      };

      histSize = 10000;

      shellInit = ''
        export PATH="$HOME/.local/bin:$PATH"

        # Setup GPG agent and SSH auth socket
        #export GPG_TTY="$(tty)"
        #export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"

        eval $(/run/wrappers/bin/gnome-keyring-daemon --start --daemonize)
        export SSH_AUTH_SOCK

        gpgconf --launch gpg-agent
        gpg-connect-agent updatestartuptty /bye >/dev/null

        # Tmux setup
        export TERM="xterm-256color"
        if [ -z "$TMUX" ] && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]]; then
          sleep 0.5
          exec tmux new-session -A -s main
        fi
      '';
    };

    fzf = {
      keybindings = true;
      fuzzyCompletion = true;
    };
  };

  environment = {systemPackages = with pkgs; [bat eza zoxide];};
}
