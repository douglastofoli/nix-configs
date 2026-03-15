{
  flake.modules.editors.nixvim =
    { pkgs, ... }:
    {
      plugins.lsp = {
        enable = true;

        servers = {
          lua_ls = {
            enable = true;
            settings = {
              diagnostics = {
                globals = [ "vim" ];
              };
              workspace = {
                checkThirdParty = false;
                telemetry = {
                  enable = false;
                };
                library = [
                  "\${3rd}/love2d/library"
                ];
              };
            };
          };
          cssls.enable = true;
          html.enable = true;
          nil_ls = {
            enable = true;
            settings = {
              nix.flake.autoArchive = true;
            };
            formatting.command = "nixfmt";
          };
          expert = {
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
            settings = { };
          };
        };

        extraPackages = with pkgs; [
          beamPackages.expert
        ];
      };
    };
}
