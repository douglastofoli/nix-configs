{ pkgs, user, ... }:

let pythonPkgs = p: with p; [ requests ];
in {
  environment = {
    systemPackages = with pkgs; [ (python3.withPackages pythonPkgs) ];
  };
}
