module XMonad.Custom.XMobar where

import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.StackSet as W

import XMonad.Custom.Colors.Dracula as C
import XMonad.Custom.Variables as C
import XMonad.Custom.Workspaces as C

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = xmobarColor C.color06 "" . wrap "[" "]"
    , ppVisible = xmobarColor C.color05 ""
    , ppHidden = xmobarColor C.color04 "" . wrap ("<fc=" ++ C.color05 ++ ">") "</fc>"
    , ppHiddenNoWindows = xmobarColor "#666666" "" 
    , ppTitle = xmobarColor C.color14 "" . shorten 60
    , ppSep =  "<fc=" ++ C.color09 ++ "> <fn=1>|</fn> </fc>"
    , ppUrgent = xmobarColor C.color02 "" . wrap "!" "!"
    , ppExtras = [windowCount]
    , ppOrder  = \(ws:l:t:ex) -> ["<fn=4>" ++ ws ++ "</fn>"] ++ ex ++ ["<fc=" ++ C.color06 ++ ">[" ++ l ++ "]</fc> " ++ t ]
    }
  where
    windowCount :: X (Maybe String)
    windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
