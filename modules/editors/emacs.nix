{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    defaultEditor = true;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    emacs28Packages.editorconfig
    fd
    jq
    nixfmt
    python310Packages.grip # Preview Markdown
    ripgrep

    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: [ epkgs.vterm ]))
  ];
}
