{ pkgs ? import <nixpkgs> { } }:

rec {
  postgresFromDockerHub = pkgs.dockerTools.pullImage {
    imageName = "postgres";
    imageDigest =
      "sha256:a5e89e5f2679863bedef929c4a7ec5d1a2cb3c045f13b47680d86f8701144ed7";
    sha256 = "sha256-GjQaNy8HCDhSjHLgvXa6DLnPXTBZOH7Ib1hBLZRvrGw=";
    finalImageTag = "15.4";
    finalImageName = "postgres-15.4";
  };

  postgres = pkgs.dockerTools.buildImage {
    name = "postgres";
    tag = "15.4";
    created = "now";
    fromImage = postgresFromDockerHub;

    runAsRoot = ''
      #!${pkgs.runtimeShell}
      mkdir -p /data
    '';

    config = {
      Cmd = [ "postgres" ];
      WorkingDir = "/data";
      Volumes = { "/data" = { }; };
    };
  };
}
