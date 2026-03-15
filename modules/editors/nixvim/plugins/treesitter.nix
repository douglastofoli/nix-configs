{
  flake.modules.editors.nixvim =
    { pkgs, ... }:
    {
      plugins.treesitter = {
        enable = true;
        nixvimInjections = true;
        folding.enable = false;
        nixGrammars = true;
        grammarPackages = pkgs.vimPlugins.nvim-treesitter.passthru.allGrammars;
        settings = {
          ensure_installed = "all";
          highlight.enable = true;
          incremental_selection.enable = true;
          indent.enable = true;
        };
      };
    };
}
