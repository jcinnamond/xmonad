{-|
  Module       : XMonad.Config.Scratchpads
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  Define scratchpads. This is used to set up manageHook, and for keybindings.
-}
module XMonad.Config.Scratchpads (
  myScratchpads
  ) where

import XMonad (title, (=?))
import qualified XMonad.StackSet as W

import XMonad.Util.NamedScratchpad (customFloating, NamedScratchpad(NS))

myScratchpads =
  [ NS "popTerm" "alacritty --title 'popTerm'"
    (title =? "popTerm")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]
