module Main (main) where

import XMonad (xmonad, def, logHook, layoutHook, spawn, terminal)

import XMonad.Hooks.DynamicLog (dynamicLogString, xmobarPP, xmonadPropLog)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)

import XMonad.Layout (Tall(..))

main :: IO ()
main = do
  spawn "~/.xmonad/bin/xmobar"
  xmonad $ docks $ def {
    terminal = "termite",
    logHook = dynamicLogString xmobarPP >>= xmonadPropLog,
    layoutHook = avoidStruts $ Tall 1 (1/100) (1/2)
    }
