{ pkgs, ... }:

with pkgs;

let projectName = "java";
in mkShell {
  name = "${projectName}-shell";

  packages = [ clang-tools jdk ];
}
