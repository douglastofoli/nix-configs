{
  flake.modules.nixos.desktop =
    {
      pkgs,
      ...
    }:
    {
      environment = {
        systemPackages = with pkgs; [
          code-cursor
          cursor-cli
        ];
      };
    };
}
