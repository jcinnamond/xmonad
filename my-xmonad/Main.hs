{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import XMonad (xmonad, def, logHook, layoutHook, workspaces, spawn, terminal, windows, X, XConfig, Window)
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog (dynamicLogString, xmobarPP, xmonadPropLog)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)

import XMonad.Layout ((|||), Tall(..), Full(..))
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.Circle (Circle(..))
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.IfMax (IfMax(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.Spacing (spacingRaw, Border(..))

import XMonad.Prompt (autoComplete, searchPredicate, sorter, XPConfig)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Shell (shellPrompt)

import XMonad.Util.EZConfig (additionalKeysP)

main :: IO ()
main = do
  xmonad $ docks $ myConfig

myConfig = def { terminal   = "termite"
               , logHook    = dynamicLogString xmobarPP >>= xmonadPropLog
               , layoutHook = myLayout
               , workspaces = myWorkspaces
               }
           `additionalKeysP` (myKeys ++ switchWorkspaceKeys)

myPromptConfig :: XPConfig
myPromptConfig = def { autoComplete = Just 200_000
                     , searchPredicate = fuzzyMatch
                     , sorter          = fuzzySort
                     }

myKeys :: [(String, X ())]
myKeys = [
  -- Prompts
    ("M4-<Return>", shellPrompt myPromptConfig)

  -- Some multimedia keys
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  ]

myWorkspaces :: [String]
myWorkspaces = ["dev", "web", "chat", "x", "y", "config", "me", "music"]

homeRow :: [String]
homeRow = ["a", "s", "d", "f", "g", "j", "k", "l"]

switchWorkspaceKeys :: [(String, X ())]
switchWorkspaceKeys = [(maybeShift ++ "M4-" ++ key, action tag)
                      | (tag, key) <- zip myWorkspaces homeRow
                      , (maybeShift, action) <- [("", windows . W.greedyView)
                                                ,("S-", windows . W.shift)]]


myLayout = avoidStruts $ spaceWindows $
           IfMax 1 Circle (combineTwo (TwoPane 0.7 0.3) Accordion Full)
           ||| Tall 1 (1/100) (1/2)
           ||| Full
  where spaceWindows = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True
