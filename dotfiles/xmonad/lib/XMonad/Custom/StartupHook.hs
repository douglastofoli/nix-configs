module XMonad.Custom.StartupHook (myStartupHook) where

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce

import qualified XMonad.Custom.Colors.Dracula as C

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"

  spawnOnce "telegram-desktop"
  spawnOnce "feh --bg-scale $HOME/.wallpaper.png"
  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ C.colorTrayer ++ " --height 24")

  setWMName "XMonad"