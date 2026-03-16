{
  config,
  inputs,
  ...
}:

let
  packages = pkgs: [ inputs.nixnvim.packages.${pkgs.system}.nix-nvim ];

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
      nixnvim-pkg = inputs.nixnvim.packages.${pkgs.system}.nix-nvim;
    in
    {
      packages.neovim = nixnvim-pkg;

      apps.neovim = {
        type = "app";
        program = "${nixnvim-pkg}/bin/nvim";
      };
    };

  flake.modules.nixos.nixnvim =
    { pkgs, ... }:
    {
      environment = environment pkgs;
    };

  flake.modules.homeManager.nixnvim =
    { pkgs, ... }:
    {
      home.packages = packages pkgs;

      home.sessionVariables = {
        EDITOR = "nvim";
        VISUAL = "nvim";
      };
    };

  flake.modules.darwin.nixnvim =
    { pkgs, ... }:
    {
      environment = environment pkgs;
    };
}
