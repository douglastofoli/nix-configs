{
  flake.modules.editors.nixvim =
    { pkgs, ... }:
    {
    plugins.conform = {
      enable = true;
      settings = {
        formatters_by_ft = {
          elixir = [ "mix" ];
          heex = [ "mix" ];
          eelixir = [ "mix" ];
          nix = [ "nixfmt" ];
          lua = [ "stylua" ];
          css = [ "prettier" ];
          html = [ "prettier" ];
          json = [ "prettier" ];
          yaml = [ "prettier" ];
          markdown = [ "prettier" ];
          javascript = [ "prettier" ];
          typescript = [ "prettier" ];
          typescriptreact = [ "prettier" ];
        };
      };
    };

    extraPackages = with pkgs; [
      nixfmt
      stylua
      prettier
    ];
  };
}