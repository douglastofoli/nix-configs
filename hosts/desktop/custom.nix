{
  pkgs,
  vars,
  ...
}: {
  alacritty = {
    enable = true;
    fontFamily = "JetBrainsMono Nerd Font";
    fontSize = 13;
  };

  firefox = {
    enable = true;
  };

  git = {
    enable = true;
    defaultBranch = "main";
    delta.enable = true;
    lfs.enable = true;
    core = {
      autocrlf = "input";
      editor = "${vars.editor}";
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
      clojure.enable = false;
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

  themes = {
    fontFamily = "Cantarell";
    wallpaper = ../../modules/themes/wallpaper5.jpg;
  };

  tmux = {
    enable = true;
    tmuxinator = true;
  };

  wezterm = {
    enable = true;
    enableBashIntegration = false;
    enableZshIntegration = true;
    fontFamily = "JetBrainsMono Nerd Font";
    fontSize = 13.0;
  };

  zsh = {
    enable = true;
  };
}
