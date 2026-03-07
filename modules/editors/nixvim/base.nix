{
  flake.modules.editors.nixvim =
    { lib, pkgs, ... }:
    {
      viAlias = true;
      vimAlias = true;

      opts = {
        number = true;
        relativenumber = true;

        shiftwidth = 2;
        tabstop = 2;
        softtabstop = 2;
        expandtab = true;

        smartindent = true;
        wrap = false;

        ignorecase = true;
        smartcase = true;

        termguicolors = true;
        cursorline = true;
        signcolumn = "yes";

        splitbelow = true;
        splitright = true;

        updatetime = 250;
        timeoutlen = 300;

        scrolloff = 8;
        sidescrolloff = 8;

        undofile = true;
      };

      plugins.web-devicons.enable = true;
    };
}
