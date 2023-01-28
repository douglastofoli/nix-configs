{ config, location, ... }:

{
  system.userActivationScripts = {
    scripts = {
      text = ''
        SCRIPTS="$HOME/.local/bin"

        if [ ! -d "$SCRIPTS" ]; then
          ln -sf ${location}/dotfiles/local/bin $SCRIPTS
        fi
      '';
    };

    xmobar = {
      text = ''
        XMOBAR="$HOME/.config/xmobar"

        if [ ! -d "$XMOBAR" ]; then
          ln -sf ${location}/dotfiles/xmobar $XMOBAR
        fi
      '';
    };

    xmonad = {
      text = ''
        XMONAD="$HOME/.config/xmonad"

        if [ ! -d "$XMONAD" ]; then
          ln -sf ${location}/dotfiles/xmonad $XMONAD
        fi
      '';
    };
  };
}
