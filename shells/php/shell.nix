{pkgs, ...}:
with pkgs; let
  projectName = "php";

  php = pkgs.php83.buildEnv {
    extensions = {
      enabled,
      all,
    }:
      enabled ++ (with all; [xdebug php83Extensions.pgsql]);

    extraConfig = ''
      memory_limit = 2G
      xdebug.mode=debug
    '';
  };
in
  mkShell {
    name = "${projectName}-shell";

    packages = [php php83Packages.composer];
  }
