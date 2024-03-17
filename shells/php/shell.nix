{ pkgs, ... }:

with pkgs;

let
  projectName = "php";

  php = pkgs.php82.buildEnv {
    extensions = ({ enabled, all }:
      enabled ++ (with all; [ xdebug php82Extensions.pgsql ]));

    extraConfig = ''
      memory_limit = 2G
      xdebug.mode=debug
    '';
  };
in mkShell {
  name = "${projectName}-shell";

  packages = [ php ];
}
