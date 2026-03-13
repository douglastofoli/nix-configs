{
  flake.modules.nixos.base = {
    i18n = {
      defaultLocale = "en_US.UTF-8";

      supportedLocales = [
        "en_US.UTF-8/UTF-8"
        "pt_BR.UTF-8/UTF-8"
      ];
    };
  };
}
