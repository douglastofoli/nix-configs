{ config, location, ... }:

{
  system.userActivationScripts = {
    source = {
      text = ''
        source ${config.system.build.setEnvironment}
      '';
    };

    emacs = {
      text = ''
        DOOM="$HOME/.doom.d"
        $EMACS="$HOME/.emacs.d"

        if [ ! -d "$EMACS" ]; then
          git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
          yes | $EMACS/bin/doom install

          rm -r $DOOM
          ln -s ${location}/dotfiles/doom.d $DOOM
          $EMACS/bin/doom sync
        else
          $EMACS/bin/doom sync
        fi

        
      '';
    };

    xmobar = {
      text = ''
        XMOBAR="$HOME/.config/xmobar"

        if [ ! -d "$XMOBAR" ]; then
          ln -s ${location}/dotfiles/xmobar $XMOBAR
        fi
      '';
    };

    xmonad = {
      text = ''
        XMONAD="$HOME/.config/xmonad"

        if [ ! -d "$XMONAD" ]; then
          ln -s ${location}/dotfiles/xmonad $XMONAD
        fi
      '';
    };
  };
}
