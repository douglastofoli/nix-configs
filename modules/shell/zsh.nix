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

    direnv = {
      enable = true;
      enableZshIntegration = config.programs.zsh.enable;
      nix-direnv.enable = true;
    };

    zsh = {
      enable = true;

      oh-my-zsh = {
        enable = true;
        plugins = [ "git" ];
      };

      plugins = [
        {
          name = "zsh-autocomplete";
          src = pkgs.fetchFromGitHub {
            owner = "marlonrichert";
            repo = "zsh-autocomplete";
            rev = "main";
            sha256 = "sha256-+w9+d7cYmPBdnqWgooh+OmscavB9JL7dVqOQyj8jJ7E=";
          };
        }
        {
          name = "zsh-autosuggestions";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-autosuggestions";
            rev = "master";
            sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w";
          };
        }
        {
          name = "fast-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zdharma-continuum";
            repo = "fast-syntax-highlighting";
            rev = "master";
            sha256 = "sha256-hMzeCRJD7njkUiSf3xT/vQ0wEkmgV53yDJTV/QZtJU4=";
          };
        }
        {
          name = "zsh-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "master";
            sha256 = "sha256-kWgPe7QJljERzcv4bYbHteNJIxCehaTu4xU9r64gUM4=";
          };
        }
      ];

      history.size = 5000;
    };
  };
}
