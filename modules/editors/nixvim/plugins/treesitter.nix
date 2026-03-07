{
  flake.modules.editors.nixvim = {
    plugins.treesitter = {
      enable = true;
      nixvimInjections = true;
    };
  };
}
