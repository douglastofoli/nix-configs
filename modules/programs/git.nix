{
  flake.modules.nixos.git =
    { pkgs, ... }:
    {
      programs = {
        git.enable = true;
      };
    };

  flake.modules.homeManager.git =
    { pkgs, ... }:
    {
      programs.git = {
        enable = true;
        lfs.enable = true;

        ignores = [
          "*.swp"
          "*.swo"
          ".nix-*"
          ".postgres"
          ".direnv"
        ];

        signing = {
          key = "2E29572E78E90F6E";
          signByDefault = true;
          signer = "${pkgs.gnupg}/bin/gpg";
        };

        settings = {
          user = {
            name = "douglastofoli";
            email = "tofoli.douglas@hotmail.com";
          };

          alias = {
            p = "push";
            s = "status";
            c = "commit";
            co = "checkout";
            aa = "add -p";
            st = "stash";
            br = "branch";
            lg = "log --graph --oneline --decorate --abbrev-commit";
          };

          github.user = "douglastofoli";
          init.defaultBranch = "main";

          grep = {
            linenumber = true;
          };
          merge = {
            log = true;
          };
          rebase = {
            autosquash = true;
          };
          fetch = {
            prune = true;
          };
          pull = {
            rebase = false;
          };
          push = {
            default = "current";
          };
          apply = {
            whitespace = "nowarn";
          };
          help = {
            autocorrect = 0;
          };

          color = {
            grep = "auto";
            branch = "auto";
            diff = "auto";
            status = "auto";
            showbranch = "auto";
            interactive = "auto";
            ui = "auto";
          };

          core = {
            autocrlf = "input";
            editor = "nvim";
            whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
          };

          url = {
            "git@github.com" = {
              insteadOf = "git://github.com/";
            };
          };
        };
      };

      programs.delta = {
        enable = true;
        enableGitIntegration = true;
      };
    };
}
