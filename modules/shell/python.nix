{ pkgs, user, ... }:

let pythonPkgs = p: with p; [ pynvim requests ];
in {
  environment = {
    systemPackages = with pkgs; [ (python3.withPackages pythonPkgs) ];
  };
}
