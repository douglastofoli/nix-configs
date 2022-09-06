{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    emacs28Packages.editorconfig
    python310Packages.grip # Preview Markdown
    coreutils
    fd
    nixfmt
    ripgrep
  ];
}
