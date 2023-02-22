module XMonad.Custom.Variables where

import XMonad
import XMonad.StackSet as W

import qualified XMonad.Custom.Colors.Dracula as C

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:regular:size=14:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty -t alacritty"

myTerminalRaw :: String
myTerminalRaw = "alacritty"

myBrowser :: String
myBrowser = "firefox "

myFileManager :: String
myFileManager = "pcmanfm"

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = C.color01

myFocusColor :: String
myFocusColor = C.color06

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
