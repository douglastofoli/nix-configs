{
  flake.modules.nixos.base = {
    services.ollama = {
      enable = false;
      loadModels = [
        "llama3.2:3b"
        "deepseek-r1:1.5b"
        "deepseek-r1:8b"
      ];
      host = "0.0.0.0";
      port = 11434;
      openFirewall = true;
    };
  };
}
