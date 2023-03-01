module XMonad.Custom.Scratchpads (myScratchPads) where

import XMonad
import XMonad.Custom.Variables qualified as C
import XMonad.Custom.Workspaces qualified as C
import XMonad.ManageHook (className, doShift, title)
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "ncmpcpp" spawnMus findMus manageMus,
    NS "calculator" spawnCalc findCalc manageCalc,
    NS "fm" spawnFM findFM manageFM
  ]
  where
    spawnTerm = C.myTerminalRaw ++ " -t tscratchpad"
    findTerm = title =? "tscratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMus = C.myTerminalRaw ++ " -t mus-scratchpad -e ncmpcpp"
    findMus = title =? "mus-scratchpad"
    manageMus = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnCalc = "qalculate-gtk"
    findCalc = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
      where
        h = 0.5
        w = 0.4
        t = 0.75 - h
        l = 0.70 - w
    spawnFM = C.myFileManager
    findFM = className =? "pcmanfm-qt"
    manageFM = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
