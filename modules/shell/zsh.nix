{ lib, pkgs, ... }:

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
      # ohMyZsh = {
      #   enable = true;
      #   plugins = [ "git"
      #     {
      #       name = "zsh-autocomplete";
      #       src = pkgs.fetchFromGitHub {
      #         owner = "marlonrichert";
      #         repo = "zsh-autocomplete";
      #         rev = "5cc9da132e7535a540fb1235ce27fd5a233d4f0e";
      #         sha256 = lib.fakeSha256;
      #       };
      #     }
      #     {
      #        name = "zsh-autosuggestions";
      #        src = pkgs.fetchFromGitHub {
      #          owner = "zsh-users";
      #          repo = "zsh-autosuggestions";
      #          rev = "a411ef3e0992d4839f0732ebeb9823024afaaaa8";
      #          sha256 = lib.fakeSha256;
      #        };
      #     }
      #     {
      #        name = "fast-syntax-highlighting";
      #        src = pkgs.fetchFromGitHub {
      #          owner = "zdharma-continuum";
      #          repo = "fast-syntax-highlighting";
      #          rev = "5521b083f8979ad40be2137d7a46bfa51c8d666a";
      #          sha256 = lib.fakeSha256;
      #        };
      #     }
      #     {
      #        name = "zsh-syntax-highlighting";
      #        src = pkgs.fetchFromGitHub {
      #          owner = "zsh-users";
      #          repo = "zsh-autosuggestions";
      #          rev = "754cefe0181a7acd42fdcb357a67d0217291ac47";
      #          sha256 = lib.fakeSha256;
      #        };
      #     }
      #   ];
      # };
      histSize = 5000;

      shellInit = ''
        export GPG_TTY=$(tty)
      '';
    };
  };
}
