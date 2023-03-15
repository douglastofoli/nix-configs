{ pkgs, javascriptDeps, elixir, ... }:

with pkgs;

let projectName = "elixir";
in mkShell {
  name = "${projectName}-shell";

  packages = [ elixir glibcLocales ] ++ javascriptDeps;
}
