{ pkgs, ... }:

with pkgs;

let pythonPackages = p: with p; [ requests ];
in {
  environment.systemPackages =
    [ python3 (python3.withPackages pythonPackages) ];
}
