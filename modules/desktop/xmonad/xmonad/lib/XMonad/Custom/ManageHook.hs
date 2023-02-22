module XMonad.Custom.ManageHook (myManageHook) where

import XMonad
import qualified XMonad.Custom.Workspaces as C
import qualified XMonad.Custom.Scratchpads as C
import XMonad.ManageHook (doShift, className, title)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.NamedScratchpad
import Data.Monoid
import XMonad.Core

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"                   --> doFloat
     , className =? "file_progress"             --> doFloat
     , className =? "dialog"                    --> doFloat
     , className =? "download"                  --> doFloat
     , className =? "error"                     --> doFloat
     , className =? "notification"              --> doFloat
     , className =? "pinentry-gtk-2"            --> doFloat
     , className =? "splash"                    --> doFloat
     , className =? "toolbar"                   --> doFloat
     , className =? "TelegramDesktop"           --> doFloat
     , className =? "Pavucontrol"               --> doCenterFloat
     , title     =? "Bluetooth"                 --> doCenterFloat
     , title     =? "emacs-run-launcher"        --> doCenterFloat
     , title     =? "emacs-web-page-selector"   --> doCenterFloat
     , className =? "Yad"                       --> doCenterFloat
     , title     =? "Mozilla Firefox"           --> doShift (C.myWorkspaces !! 1)
     , className =? "TelegramDesktop"           --> doShift (C.myWorkspaces !! 3)
     , className =? "discord"                   --> doShift (C.myWorkspaces !! 4)
     , className =? "YouTube Music"             --> doShift (C.myWorkspaces !! 5)
     , className =? "Gimp"                      --> doShift (C.myWorkspaces !! 7)
     , className =? "zoom"                      --> doShift (C.myWorkspaces !! 6)
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
     , isFullscreen -->  doFullFloat
     ]
     <+> namedScratchpadManageHook C.myScratchPads
     <+> manageDocks
