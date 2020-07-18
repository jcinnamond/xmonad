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
focusedColor = "#de935f"
focusedBackgroundColor = "#a54242"
activeColor = "#5e8d87"
inactiveColor = "#707880"

foregroundColor = "#c5c8c6"
overlayBackgroundColor = "#384d5d"

theme = def { borderWidth = 2
            , focusedBorderColor = focusedBackgroundColor
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
