{
  flake.modules.editors.nixvim =
    { lib, pkgs, ... }:
    {
      enableMan = false;
      viAlias = true;
      vimAlias = true;

      colorschemes.dracula.enable = true;

      opts = {
        number = true;
        relativenumber = true;
        shiftwidth = 2;
        tabstop = 2;
        expandtab = true;
        smarttab = true;
        undofile = true;
      };

      clipboard = {
        register = "unnamedplus";
        providers.wl-copy = lib.mkIf pkgs.stdenv.isLinux {
          enable = true;
        };
      };
    };
}
