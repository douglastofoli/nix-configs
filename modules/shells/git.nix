{
  custom-config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = custom-config.git;
in {
  options.git = {
    enable = mkEnableOption {
      description = "Enables Git";
      type = types.bool;
      default = false;
    };

    core = {
      autocrlf = mkOption {
        description = "Defines if enables autocrlf";
        type = types.bool or types.str;
        default = false;
      };
      editor = mkOption {
        description = "Defines the default editor";
        type = types.str;
        default = "";
      };
      whitespace = mkOption {
        description = "Defines rules for formatting and whitespaces";
        type = types.str;
        default = "";
      };
    };

    defaultBranch = mkOption {
      description = "Defines the default branch";
      type = types.str;
      default = "main";
    };

    delta.enable = mkEnableOption {
      description = "Enables a git diff tool";
      type = types.bool;
      default = false;
    };

    lfs.enable = mkEnableOption {
      description = "Enables Git Large File Storage (LFS)";
      type = types.bool;
      default = false;
    };

    user = {
      name = mkOption {
        description = "The git username";
        type = types.str;
        default = "";
      };
      email = mkOption {
        description = "The git email";
        type = types.str;
        default = "";
      };
    };

    signing = {
      key = mkOption {
        description = "Defines the GPG key";
        type = types.str;
        default = "";
      };
      signByDefault = mkOption {
        description = "Defines if will sign commits by default";
        type = types.str;
        default = false;
      };
      gpgPath = mkOption {
        description = "Defines the GPG path";
        type = types.str;
        default = "${pkgs.gnupg}/bin/gpg";
      };
    };
    signingkey = mkOption {
      description = "The SSH signing key";
      type = types.str;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      inherit (cfg) enable delta lfs;

      userName = cfg.user.name;
      userEmail = cfg.user.email;
      ignores = ["*.swp" "*.swo" ".nix-*" ".postgres" ".direnv"];

      signing = {
        inherit (cfg.signing) gpgPath key signByDefault;
      };

      aliases = {
        p = "push";
        s = "status";
        c = "commit";
        co = "checkout";
        aa = "add -p";
        st = "stash";
        br = "branch";
        lg = "log --graph --oneline --decorate --abbrev-commit";
      };

      extraConfig = {
        github.user = cfg.user.name;
        init.defaultBranch = cfg.defaultBranch;

        grep = {linenumber = true;};
        merge = {log = true;};
        rebase = {autosquash = true;};
        fetch = {prune = true;};
        pull = {rebase = false;};
        push = {default = "current";};
        apply = {whitespace = "nowarn";};
        help = {autocorrect = 0;};

        commit = {
          template = builtins.toPath ../../dotfiles/gitmessage;
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
          autocrlf = cfg.core.autocrlf;
          editor = cfg.core.editor;
          whitespace = cfg.core.whitespace;
        };

        url = {
          "git@github.com" = {
            insteadOf = "git://github.com/";
          };
        };
      };
    };
  };
}
