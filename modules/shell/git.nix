{ pkgs, ... }:

let
  username = "douglastofoli";
  email = "tofoli.douglas@hotmail.com";
in {
  programs.git = {
    enable = true;
    userName = "${username}";
    userEmail = "${email}";
    signing = {
      gpgPath = "${pkgs.gnupg}/bin/gpg2";
      key = "";
      signByDefault = true;
    };
    aliases = {
      p = "push";
      s = "status";
      c = "commit";
      b = "branch";
      lg = "log --graph --oneline --decorate --abbrev-commit";
    };
    extraConfig = {
      github = { user = "${username}"; };
      push = { default = "current"; };
      init = { defaultBranch = "main"; };
      pull = { rebase = false; };
      rebase = { autosquash = true; };
      log = {
        follow = true;
        abbrevCommit = true;
      };
      core = { editor = "emacs"; };
      color = {
        grep = "auto";
        branch = "auto";
        diff = "auto";
        status = "auto";
        showBranch = "auto";
        interactive = "auto";
        ui = "auto";
      };
    };
  };
}
