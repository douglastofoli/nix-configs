{pkgs, ...}: let
  inherit (pkgs) mkNeovim;

  config = {
    nvim = {
      package = pkgs.neovim-unwrapped;

      ui = {
        statusline = {
          enable = true;
          theme = "dracula";
        };
        whichkey.enable = true;
      };
    };
  };
in
  mkNeovim {inherit config;}
# programs.neovim = {
#   enable = true;
#   defaultEditor = false;
#   vimAlias = true;
# };

