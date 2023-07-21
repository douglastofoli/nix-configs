{ config, pkgs, ... }:

{
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

        gcloud = { detect_env_vars = [ "GCLOUD_HOME" ]; };
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
        export GPG_TTY="$(tty)"
        export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
        gpg-connect-agent updatestartuptty /bye > /dev/null
      '';
    };

    fzf = {
      keybindings = true;
      fuzzyCompletion = true;
    };
  };

  environment = { systemPackages = with pkgs; [ bat exa zoxide ]; };
}
