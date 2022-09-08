module XMonad.Custom.XMobar where

import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

import XMonad.Custom.Colors.Dracula as C
import XMonad.Custom.Variables as C
import XMonad.Custom.Workspaces as C

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppTitle           = xmobarColor C.color16 "" . shorten 60
    , ppCurrent         = xmobarColor C.color06 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ C.color06 ++ ">") "</box>"
    , ppVisible         = xmobarColor C.color06 "" . C.clickable
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = xmobarColor C.color02 "" . wrap "!" "!"
    , ppOrder           = \(ws:l:t:ex) -> ["<fn=4>" ++ ws ++ "</fn>"] ++ ex ++ ["<fc=" ++ C.color06 ++ ">[" ++ l ++ "]</fc> " ++ t]
    , ppExtras          = [C.windowCount]
    }
  where
    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bfbfbf" ""
