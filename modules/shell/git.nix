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
        signingkey = "4551DDB13FA45EA0";
      };
      core = { editor = "emacs"; };
      init = { defaultBranch = "main"; };
      commit = { gpgsign = true; };
      pull = { rebase = false; };
    };
  };
}
