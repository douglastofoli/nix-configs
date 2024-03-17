{
  pkgs,
  vars,
  ...
}: {
  home-manager.users.${vars.user} = {
    home.packages = [
      # (pkgs.dwarf-fortress-packages.dwarf-fortress-full.override {
      #   dfVersion = "0.47.05";
      #   theme = "obsidian";
      #   enableFPS = true;
      #   enableIntro = true;
      # })
    ];
  };
}
