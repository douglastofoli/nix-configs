{
  config,
  inputs,
  ...
}:

let
  nixvimConfig = pkgs: {
    enable = true;
    nixpkgs.pkgs = pkgs;
    imports = [ config.flake.modules.editors.nixvim ];
  };

  environment = pkgs: {
    systemPackages = with pkgs; [
      fd
      ripgrep
    ];
    variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
in
{
  perSystem =
    { inputs', pkgs, ... }:
    {
      packages.neovim = inputs'.nixvim.legacyPackages.makeNixvimWithModule {
        inherit pkgs;
        module = config.flake.modules.editors.nixvim;
      };
    };

  flake.modules.nixos.nixvim =
    { pkgs, ... }:
    {
      imports = [
        inputs.nixvim.nixosModules.nixvim
      ];
      programs.nixvim = nixvimConfig pkgs;
      environment = environment pkgs;
    };

  flake.modules.darwin.nixvim =
    { pkgs, ... }:
    {
      imports = [
        inputs.nixvim.nixDarwinModules.nixvim
      ];
      programs.nixvim = nixvimConfig pkgs;
      environment = environment pkgs;
    };
}
