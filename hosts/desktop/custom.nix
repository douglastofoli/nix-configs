{pkgs, ...}: {
  helix = {
    enable = true;
    languages = {
      css.enable = true;
      elixir.enable = true;
      html.enable = true;
      json.enable = true;
      nix.enable = true;
    };
  };
}
