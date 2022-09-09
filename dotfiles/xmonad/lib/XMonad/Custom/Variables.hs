module XMonad.Custom.Variables where

import XMonad

import qualified XMonad.Custom.Colors.Dracula as C

myFont :: String
myFont = "xft:RobotoMono Nerd Font:regular:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myFileManager :: String
myFileManager = "pcmanfm"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs'"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = C.color01

myFocusedBorderColor :: String
myFocusedBorderColor = C.color06
