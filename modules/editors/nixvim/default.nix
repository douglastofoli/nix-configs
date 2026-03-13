{
  config,
  inputs,
  ...
}:

let
  packages = pkgs: [
    inputs.nixnvim.packages.${pkgs.system}.nix-nvim
    pkgs.ripgrep
  ];

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
    {
      packages.neovim = inputs.nixnvim.packages.${pkgs.system}.nix-nvim;
      packages.nix-nvim = inputs.nixnvim.packages.${pkgs.system}.nix-nvim;
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
