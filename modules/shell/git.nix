{ pkgs, ... }:

let
  username = "douglastofoli";
  email = "tofoli.douglas@hotmail.com";
in {
  programs.git = {
    enable = true;
    config = {
      user = {
        name = username;
        email = email;
        # signingkey = "5DF64272B34553CA";
      };
      core = { editor = "emacs"; };
      init = { defaultBranch = "main"; };
      # commit = { gpgsign = true; };
      pull = { rebase = false; };
    };
  };
}
