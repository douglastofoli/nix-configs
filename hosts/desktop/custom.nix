{pkgs, ...}: {
  alacritty = {
    enable = true;
    fontFamily = "JetBrainsMono Nerd Font";
    fontSize = 13;
  };

  git = {
    enable = true;
    defaultBranch = "main";
    delta.enable = true;
    lfs.enable = true;
    core = {
      autocrlf = "input";
      editor = "${pkgs.helix}/bin/hx";
      whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
    };
    user = {
      name = "douglastofoli";
      email = "tofoli.douglas@hotmail.com";
    };
    signing = {
      key = "A30D5C3DE5FCB642";
      signByDefault = true;
      gpgPath = "${pkgs.gnupg}/bin/gpg";
    };
  };

  helix = {
    enable = true;
    languages = {
      clojure.enable = true;
      css.enable = true;
      elixir.enable = true;
      haskell.enable = true;
      html.enable = true;
      javascript.enable = true;
      json.enable = true;
      markdown.enable = true;
      nix.enable = true;
    };
  };

  lf = {
    enable = true;
  };
}
