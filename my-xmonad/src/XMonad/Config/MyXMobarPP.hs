{-|
  Module       : XMonad.Config.MyXmobarPP
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  This module defines the pretty printer for XMobar
-}
module XMonad.Config.MyXMobarPP (
  myXMobarPP
  ) where

import XMonad.Hooks.DynamicLog (PP(..), wrap, xmobarColor, xmobarPP)

myXMobarPP :: PP
myXMobarPP = xmobarPP { ppCurrent = xmobarColor "#de935f" "" . wrap "[" "]"
                      , ppHidden = xmobarColor "#c5c8c6" ""
                      , ppHiddenNoWindows = xmobarColor "#707880" ""
                      , ppTitle = xmobarColor "#c5c8c6" ""
                      , ppSep = xmobarColor "#707880" "" "   |||   "
                      }
