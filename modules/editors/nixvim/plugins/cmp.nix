{
  flake.modules.editors.nixvim = {
    plugins = {
      cmp = {
        enable = true;
        autoEnableSources = true;
      };

      luasnip.enable = true;
    };
  };
}
