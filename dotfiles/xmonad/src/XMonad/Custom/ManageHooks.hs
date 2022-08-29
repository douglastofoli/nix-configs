module XMonad.Custom.ManageHook (myManageHook) where

import XMonad
import XMonad.Core

import Data.Monoid

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook (doShift, className, title)

import XMonad.Layout.NoBorders

import XMonad.Util.NamedScratchpad

import qualified XMonad.Custom.Workspaces as C
import qualified XMonad.Custom.Scratchpads as C

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "confirm"                             --> doFloat
  , className =? "file_progress"                       --> doFloat
  , className =? "dialog"                              --> doFloat
  , className =? "download"                            --> doFloat
  , className =? "error"                               --> doFloat
  , className =? "Gimp"                                --> doFloat
  , className =? "notification"                        --> doFloat
  , className =? "pinentry-gtk-2"                      --> doFloat
  , className =? "splash"                              --> doFloat
  , className =? "toolbar"                             --> doFloat
  , className =? "TelegramDesktop"                     --> doFloat
  , title     =? "Bluetooth"                           --> doFloat
  , title     =? "emacs-run-launcher"                  --> doCenterFloat
  , title     =? "emacs-web-page-selector"             --> doCenterFloat
  , className =? "Yad"                                 --> doCenterFloat
  , className =? "firefox"                             --> doShift (C.myWorkspaces !! 0)
  , className =? "TelegramDesktop"                     --> doShift (C.myWorkspaces !! 3)
  , className =? "discord"                             --> doShift (C.myWorkspaces !! 4)
  , className =? "Spotify"                             --> doShift (C.myWorkspaces !! 5)
  , className =? "Gimp"                                --> doShift (C.myWorkspaces !! 7)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen                                       --> doFullFloat
  ] <+> namedScratchpadManageHook C.myScratchPads
    <+> manageDocks