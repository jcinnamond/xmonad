{-|
  Module       : XMonad.Config.Theme
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  This module defines colors and other UI configuration.
-}
module XMonad.Config.Theme (
  focusedColor, activeColor, inactiveColor, foregroundColor,
  myXP, theme, myTabTheme
  ) where

import XMonad (borderWidth, def, focusedBorderColor, normalBorderColor, XConfig)
import XMonad.Prompt (borderColor, bgColor, bgHLight, fgColor, fgHLight, font, height, position, promptBorderWidth, XPConfig, XPPosition(CenteredAt))

import qualified XMonad.Layout.Tabbed as Tabbed

focusedColor, activeColor, inactiveColor, foregroundColor, overlayBackgroundColor :: [Char]
-- focusedColor = "#e52a6f"
focusedColor = "#f18d9e"
focusedBackgroundColor = "#a54242"
-- activeColor = "#67aeca"
-- activeColor = "#98dbce"
activeColor = "#5bc8ad"
inactiveColor = "#707880"

foregroundColor = "#c5c8c6"
overlayBackgroundColor = "#384d5d"

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
           , height = 32
           , font = "xft:NotoSans Nerd Font:size=14:antialias=true:hinting=true"
           , position = CenteredAt 0.3 0.5
           }

myTabTheme :: Tabbed.Theme
myTabTheme = Tabbed.def { Tabbed.activeColor = focusedBackgroundColor
                        , Tabbed.activeTextColor = foregroundColor
                        , Tabbed.activeBorderWidth = 0
                        , Tabbed.inactiveColor = inactiveColor
                        , Tabbed.inactiveTextColor = foregroundColor
                        , Tabbed.inactiveBorderColor = foregroundColor
                        , Tabbed.fontName = "xft:NotoSans Nerd Font:size=9"
                        , Tabbed.decoHeight = 21
                        }
