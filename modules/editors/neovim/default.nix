{
  config,
  inputs,
  ...
}:

let
  packages = pkgs: [ inputs.neovim.packages.${pkgs.system}.neovim ];

  environment = pkgs: {
    systemPackages = packages pkgs;
    variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
in
{
  perSystem =
    { pkgs, ... }:
    let
      neovim-pkg = inputs.neovim.packages.${pkgs.system}.neovim;
    in
    {
      packages.neovim = neovim-pkg;

      apps.neovim = {
        type = "app";
        program = "${neovim-pkg}/bin/nvim";
      };
    };

  flake.modules.nixos.neovim =
    { pkgs, ... }:
    {
      environment = environment pkgs;
    };

  flake.modules.homeManager.neovim =
    { pkgs, ... }:
    {
      home.packages = packages pkgs;

      home.sessionVariables = {
        EDITOR = "nvim";
        VISUAL = "nvim";
      };
    };

  flake.modules.darwin.neovim =
    { pkgs, ... }:
    {
      environment = environment pkgs;
    };
}
