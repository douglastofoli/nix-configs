{ config, location, ... }:

{
  system.userActivationScripts = {
    source = {
      text = ''
        source ${config.system.build.setEnvironment}
      '';
    };

    doomEmacs = {
      text = ''
        DOOM="$HOME/.doom.d"
        EMACS="$HOME/.emacs.d"

        if [ ! -d "$DOOM" ]; then
          ln -s ${location}/dotfiles/doom.d $DOOM
        fi

        if [ ! -d "$EMACS" ]; then
          git clone --depth 1 https://github.com/doomemacs/doomemacs $EMACS
          yes | $EMACS/bin/doom install

          $EMACS/bin/doom sync
        fi
      '';
    };

    i3configs = {
      text = ''
        CONFIG="$HOME/.config"

        if [ ! -d "$CONFIG/i3status" ]; then
          ln -s ${location}/dotfiles/i3status $CONFIG/i3status
        fi
      '';
    };
  };
}
