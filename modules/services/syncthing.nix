{ config, lib, ... }:
let
  user =
    (config.host or {
      user = {
        name = "douglas";
      };
    }
    ).user.name;
  dataDir = "/home/${user}/Sync";
in
{
  flake.modules.nixos.base = {
    services = {
      syncthing = {
        enable = true;
        user = "${user}";
        dataDir = dataDir;

        settings = {
          devices = {
            "laptop" = {
              id = "PKQRHHC-VFLDBO2-JKBJ6WE-KLNLELK-WHMT5MW-SPLJGQP-HYXTTPY-PNPQHQK";
            };
            "xiaomi" = {
              id = "AVIFA6L-REOKPV3-QN3WQWN-S4I4JG7-XXYT7QT-5NUWAJY-EFSGM4M-PKHDBAI";
            };
          };
          folders = {
            "Obsidian" = {
              path = "/home/${user}/Obsidian";
              devices = [
                "laptop"
                "xiaomi"
              ];
            };
            "Sync" = {
              path = "/home/${user}/Sync";
              devices = [ "xiaomi" ];
            };
          };
        };
      };
    };
  };
}
