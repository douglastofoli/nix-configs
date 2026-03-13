{
  flake.modules.editors.nixvim = {
    plugins.lsp.enable = true;

    plugins.lsp.servers.expert = {
      enable = true;
      cmd = [
        "expert"
        "--stdio"
      ];
      filetypes = [
        "elixir"
        "eelixir"
        "heex"
      ];
      rootMarkers = [
        "mix.exs"
        ".git"
      ];
    };
  };
}
