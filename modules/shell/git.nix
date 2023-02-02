{ pkgs, ... }:

let
  username = "douglastofoli";
  email = "tofoli.douglas@hotmail.com";
in {
  programs.git = {
    enable = true;
    config = {
      user = {
        name = "douglastofoli";
        email = "tofoli.douglas@hotmail.com";
      };
      core = { editor = "emacs"; };
      init = { defaultBranch = "main"; };
      commit = { gpgsign = true; };
      pull = { rebase = false; };
    };
  };
}
