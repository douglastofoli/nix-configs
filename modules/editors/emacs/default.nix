{ nix-emacs, system, ... }:

let 
  emacs = nix-emacs.packages.${system}.default;
in {
  environment.systemPackages = [
    emacs
  ];
}
