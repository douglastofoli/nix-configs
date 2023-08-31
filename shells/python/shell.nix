{ pkgs, ... }:

with pkgs;

let 
  projectName = "python";
  pythonPackages = ps: with ps; [
    requests
  ];
in mkShell {
  name = "${projectName}-shell";

  packages = [
    (python3.withPackages
      pythonPackages)
  ];
}
