module Main where

import System.IO (hPutStrLn)
import XMonad
import XMonad.Custom.Colors.Dracula qualified as C
import XMonad.Custom.Keys qualified as C
import XMonad.Custom.Layouts qualified as C
import XMonad.Custom.ManageHook qualified as C
import XMonad.Custom.Startup qualified as C
import XMonad.Custom.Variables qualified as C
import XMonad.Custom.Workspaces qualified as C
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe ("xmobar $HOME/.config/xmobar/" ++ C.colorScheme ++ "-xmobarrc")
  xmonad $
    addDescrKeys' ((mod4Mask, xK_F1), C.showKeybindings) C.myKeys $
      ewmhFullscreen $
        ewmh $
          docks $
            def
              { manageHook =
                  insertPosition End Newer
                    <+> C.myManageHook,
                handleEventHook =
                  serverModeEventHookCmd
                    <+> serverModeEventHook
                    <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                    <+> minimizeEventHook,
                modMask = C.myModMask,
                startupHook = C.myStartupHook,
                layoutHook = C.myLayouts,
                workspaces = C.myWorkspaces,
                borderWidth = C.myBorderWidth,
                normalBorderColor = C.myNormColor,
                focusedBorderColor = C.myFocusColor,
                logHook =
                  dynamicLogWithPP $
                    filterOutWsPP [scratchpadWorkspaceTag] $
                      xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x,
                          ppCurrent = xmobarColor C.color06 "", -- . wrap "[" "]"
                          ppVisible = xmobarColor C.color05 "" . C.clickable,
                          ppHidden =
                            xmobarColor C.color04 ""
                              . wrap
                                ("<fc=" ++ C.color05 ++ ">")
                                "</fc>"
                              . C.clickable,
                          ppHiddenNoWindows = xmobarColor "#666666" "" . C.clickable,
                          ppTitle = xmobarColor C.color14 "" . shorten 60,
                          ppSep = "<fc=" ++ C.color09 ++ "> <fn=1>|</fn> </fc>",
                          ppUrgent = xmobarColor C.color02 "" . wrap "!" "!",
                          ppExtras = [C.windowCount],
                          ppOrder = \(ws : l : t : ex) -> ["<fn=4>" ++ ws ++ "</fn>"] ++ ex ++ ["<fc=" ++ C.color06 ++ ">[" ++ l ++ "]</fc> " ++ t]
                        }
              }
