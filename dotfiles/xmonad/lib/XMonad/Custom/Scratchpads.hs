module XMonad.Custom.Scratchpads (myScratchPads) where

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook (className, title, doShift)
import qualified XMonad.StackSet as W
import qualified XMonad.Custom.Variables as C

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                -- , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = C.myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w