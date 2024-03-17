{ pkgs, ... }:

with pkgs;

let projectName = "java";
in mkShell {
  name = "${projectName}-shell";

  packages = [ ant clang-tools gradle jdk maven ];
}
