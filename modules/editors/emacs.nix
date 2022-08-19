{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    defaultEditor = true;
  };
}
