{...}: {
  programs.ssh = {
    startAgent = false;
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };
}
