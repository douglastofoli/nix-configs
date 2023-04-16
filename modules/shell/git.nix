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
        signingkey = "";
      };
      core = { editor = "lvim"; };
      init = { defaultBranch = "main"; };
      commit = { gpgsign = true; };
      pull = { rebase = false; };
      alias = {
        ch = "checkout";
        st = "status";
        co = "commit";
        ps = "push";
        log = "log --all --graph --decorate --oneline --abbrev-commit";
        last = "log -1 HEAD";
        unstage = "reset HEAD --";
      };
    };
  };
}
