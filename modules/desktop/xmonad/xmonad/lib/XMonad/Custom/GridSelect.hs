module XMonad.Custom.GridSelect (spawnSelected', myAppGrid, myGridConfig, myColorizer) where

import XMonad
import XMonad.Custom.Variables
import XMonad.Actions.GridSelect

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x1a,0x1b,0x29) -- lowest inactive bg
                  (0x1a,0x1b,0x29) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x1a,0x1b,0x29) -- active fg

-- gridSelect menu layout
myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Terminal", myTerminal)
                 , ("Brave", "brave")
                 , ("Emacs", "emacsclient -c -a emacs")
                 , ("Firefox", "firefox")
                 , ("Gimp", "gimp")
                 , ("OBS", "obs")
                 , ("PCManFM", "pcmanfm")
                 , ("Alacritty", "alacritty")
                 , ("btop", myTerminal ++ " -e btop")
                 , ("Neovim", myTerminal ++ " -e nvim")
                 , ("Network", myTerminal ++ " -e nmtui")
                 ]
