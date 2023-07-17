{
  programs.firefox = {
    enable = true;

    profiles.default = {
      isDefault = true;
      settings = {
        "browser.tabs.inTitlebar" = 0;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      };
      userChrome = ''
        /*** Hide Tab Close buttons ***/
        .tabbrowser-tab .tab-close-button {
          visibility: collapse !important;
        }
      '';
    };
  };
}
