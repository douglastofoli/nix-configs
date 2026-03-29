{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      programs.steam = {
        enable = true;
      };
    };
}
