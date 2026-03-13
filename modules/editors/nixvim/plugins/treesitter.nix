{
  flake.modules.editors.nixvim =
    { pkgs, ... }:
    {
      plugins.treesitter = {
        enable = true;
        indent.enable = true;
        highlight.enable = true;

        grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
          bash
          json
          lua
          make
          markdown
          nix
          regex
          toml
          vim
          vimdoc
          xml
          yaml
          elixir
          heex
          eex
          html
        ];
      };
    };
}
