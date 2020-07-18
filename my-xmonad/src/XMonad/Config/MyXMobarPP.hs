{-|
  Module       : XMonad.Config.MyXMobarPP
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  This module defines the pretty printer for XMobar
-}
module XMonad.Config.MyXMobarPP (
  myXMobarPP
  ) where

import XMonad.Hooks.DynamicLog (PP(..), wrap, xmobarColor, xmobarPP)
import XMonad.Config.Theme (activeColor, focusedColor, inactiveColor, foregroundColor)

myXMobarPP :: PP
myXMobarPP = xmobarPP { ppCurrent = xmobarColor focusedColor "" . wrap "[" "]"
                      , ppHidden = xmobarColor activeColor ""
                      , ppHiddenNoWindows = xmobarColor inactiveColor ""
                      , ppTitle = xmobarColor foregroundColor ""
                      , ppSep = xmobarColor inactiveColor "" "   |   "
                      , ppLayout = \_ -> ""
                      }
