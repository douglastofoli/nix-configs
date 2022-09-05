module XMonad.Custom.GridSelect (spawnSelected', myGridConfig, myColorizer, gsCategories, gsGames, gsEducation, gsInternet, gsMultimedia, gsOffice, gsSettings, gsSystem, gsUtilities) where

import XMonad
import XMonad.Custom.Variables as C
import XMonad.Actions.GridSelect
import qualified Data.Map as M

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList 
          [ ((0, xK_Escape), cancel)
          , ((0, xK_Return), select)
          , ((0, xK_slash), substringSearch myNavigation)
          , ((0, xK_Left), move (-1, 0) >> myNavigation)
          , ((0, xK_h), move (-1, 0) >> myNavigation)
          , ((0, xK_Right), move (1, 0) >> myNavigation)
          , ((0, xK_l), move (1, 0) >> myNavigation)
          , ((0, xK_Down), move (0, 1) >> myNavigation)
          , ((0, xK_j), move (0, 1) >> myNavigation)
          , ((0, xK_Up), move (0, -1) >> myNavigation)
          , ((0, xK_k), move (0, -1) >> myNavigation)
          , ((0, xK_y), move (-1, -1) >> myNavigation)
          , ((0, xK_i), move (1, -1) >> myNavigation)
          , ((0, xK_n), move (-1, 1) >> myNavigation)
          , ((0, xK_m), move (1, -1) >> myNavigation)
          , ((0, xK_space), setPos (0, 0) >> myNavigation)
          ]
        navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x28, 0x2c, 0x34) -- lowest inactive bg
  (0x28, 0x2c, 0x34) -- highest inactive bg
  (0xc7, 0x92, 0xea) -- active bg
  (0xc0, 0xa7, 0x9a) -- inactive bg
  (0x28, 0x2c, 0x34) -- active fg

myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight    = 40
  , gs_cellwidth     = 200
  , gs_cellpadding   = 6
  , gs_navigate      = myNavigation
  , gs_originFractX  = 0.5
  , gs_originFractY  = 0.5
  , gs_font          = C.myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def
                  { gs_cellheight   = 40
                  , gs_cellwidth    = 180
                  , gs_cellpadding  = 6
                  , gs_originFractX = 0.5
                  , gs_originFractY = 0.5
                  , gs_font         = C.myFont
                  }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing -> return ()

gsCategories =
  [ ("Games",      "xdotool key super+alt+1")
  , ("Education",  "xdotool key super+alt+2")
  , ("Internet",   "xdotool key super+alt+3")
  , ("Multimedia", "xdotool key super+alt+4")
  , ("Office",     "xdotool key super+alt+5")
  , ("Settings",   "xdotool key super+alt+6")
  , ("System",     "xdotool key super+alt+7")
  , ("Utilities",  "xdotool key super+alt+8")
  ]

gsGames = 
  [ ("Steam", "steam") ]

gsEducation = []

gsInternet = 
  [ ("Discord", "discord")
  , ("Firefox", "firefox")
  , ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("OBS Stdudio", "obs")
  , ("VLC", "vlc")
  ]

gsOffice = []

gsSettings = []

gsSystem = 
  [ ("Alacritty", "alacritty")
  , ("Bash", "bash")
  , ("PCManFM", "pcmanfm")
  , ("VirtualBox", "virtualbox")
  , ("Zsh", (myTerminal ++ " -e zsh"))
  ]

gsUtilities = 
  [ ("Emacs", "emacsclient -c -a 'emacs'")
  , ("Visual Studio Code", "code")
  , ("Vim", (myTerminal ++ " -e vim"))
  ]