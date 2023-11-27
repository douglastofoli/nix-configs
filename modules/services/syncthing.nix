{vars, ...}: let
  user = vars.user;
in {
  services.syncthing = {
    enable = true;
    user = "${user}";
    dataDir = "/home/${user}/Sync";

    settings = {
      devices = {
        "xiaomi" = {
          id = "FVXA4RC-UJZHWLU-GNPDYPB-HCF2XJI-Z3Q5ARL-F5JEDJL-OCFKNCC-WQF2IAR";
        };
      };
      folders = {
        "Logseq" = {
          path = "/home/${user}/Logseq";
          devices = ["xiaomi"];
        };
        "Sync" = {
          path = "/home/${user}/Sync";
          devices = ["xiaomi"];
        };
      };
    };
  };
}
