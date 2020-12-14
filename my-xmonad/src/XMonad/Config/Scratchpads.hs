{-|
  Module       : XMonad.Config.Scratchpads
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  Define scratchpads. This is used to set up manageHook, and for keybindings.
-}
module XMonad.Config.Scratchpads (
  myScratchpads
  ) where

import XMonad (className, (=?))
import qualified XMonad.StackSet as W

import XMonad.Util.NamedScratchpad (nonFloating, customFloating, NamedScratchpad(NS))

myScratchpads =
  [ NS "popTerm" "termite --class 'popTerm'"
    (className =? "popTerm")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "pullTerm" "termite --class 'pullTerm'"
    (className =? "pullTerm")
    nonFloating
  ]
