{
  flake.modules.editors.nixvim = {
    colorschemes.dracula-nvim = {
      enable = true;
      autoLoad = true;
      settings = {
        italic_comment = true;
      };
    };
  };
}
