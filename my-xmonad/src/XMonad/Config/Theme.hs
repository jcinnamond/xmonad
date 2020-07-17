{-|
  Module       : XMonad.Config.Theme
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  This module defines colors and other UI configuration.
-}
module XMonad.Config.Theme (
  focusedColor, activeColor, inactiveColor, foregroundColor,
  myXP, theme
  ) where

import XMonad (borderWidth, def, focusedBorderColor, normalBorderColor, XConfig)
import XMonad.Prompt (borderColor, bgColor, bgHLight, fgColor, fgHLight, font, height, position, promptBorderWidth, XPConfig, XPPosition(CenteredAt))

focusedColor, activeColor, inactiveColor, foregroundColor, overlayBackgroundColor :: [Char]
focusedColor = "#ed7a71"
activeColor = "#aac0aa"
inactiveColor = "#777777"

foregroundColor = "#d0d0d0"
overlayBackgroundColor = "#404040"

theme = def { borderWidth = 2
            , focusedBorderColor = focusedColor
            , normalBorderColor = inactiveColor
            }


myXP :: XPConfig
myXP = def { fgColor = foregroundColor
           , bgColor = overlayBackgroundColor
           , fgHLight = focusedColor
           , bgHLight = overlayBackgroundColor
           , borderColor = inactiveColor
           , promptBorderWidth = 1
           , height = 28
           , font = "xft:NotoSans Nerd Font:size=14:antialias=true:hinting=true"
           , position = CenteredAt 0.3 0.5
           }
