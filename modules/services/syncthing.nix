{ vars, ... }:

let user = "${vars.user}";
in {
  home-manager.users.${vars.user} = {
    services.syncthing = {
      enable = true;
      user = "${user}";
      dataDir = "/home/${user}/Sync";
      devices = {
        "xiaomi" = {
          id =
            "3RZ7NZW-LW4JZPE-DO6XB5M-WIVTSRD-N3PQOLO-OMNYNH7-APRXQT7-INIGUAQ";
        };
      };
      folders = {
        "Obsidian" = {
          path = "/home/${user}/Obsidian";
          devices = [ "xiaomi" ];
        };
        "Sync" = {
          path = "/home/${user}/Sync";
          devices = [ "xiaomi" ];
        };
      };
    };
  };
}
