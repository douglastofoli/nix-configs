{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = false;

    vimAlias = true;
  };
}
