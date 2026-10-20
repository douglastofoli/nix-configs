{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      programs = {
        git.enable = true;
      };
    };
}
