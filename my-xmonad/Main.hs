module Main (main) where

import XMonad (keys, logHook, layoutHook, spawn, terminal, Window, windows, workspaces, X, xmonad)
import qualified XMonad.StackSet as W

import XMonad.Config.MyKeys (myKeys)
import XMonad.Config.MyXMobarPP (myXMobarPP)
import XMonad.Config.Theme (theme)

import XMonad.Hooks.DynamicLog (dynamicLogString, xmonadPropLog)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)

import XMonad.Layout ((|||), Tall(..), Full(..))
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.Constrained (Constrained(..))
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.IfMax (IfMax(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.Spacing (spacingRaw, Border(..))

import XMonad.Prompt (autoComplete, searchPredicate, sorter, XPConfig)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.EZConfig (additionalKeysP)

main :: IO ()
main = xmonad $ docks $ myConfig

myConfig = theme { keys       = myKeys
                 , logHook    = dynamicLogString myXMobarPP >>= xmonadPropLog
                 , layoutHook = myLayout
                 , workspaces = myWorkspaces
                 }
           `additionalKeysP` (switchWorkspaceKeys)

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
           constrainSingle (combineTwo (Tall 1 (1/100) (1/4)) Accordion Full)
           ||| constrainSingle (Tall 1 (1/100) (1/2))
           ||| Full
  where
    constrainSingle = IfMax 1 Constrained
    spaceWindows = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True
