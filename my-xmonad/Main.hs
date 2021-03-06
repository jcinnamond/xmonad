{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
module Main (main) where

import XMonad
    ( XConfig(keys, logHook, layoutHook, manageHook, workspaces),
      (|||),
      xmonad,
      windows,
      X ) -- (keys, logHook, layoutHook, sendMessage, spawn, Window, windows, withFocused, workspaces, X, xmonad)
import qualified XMonad.StackSet as W

import XMonad.Config.MyKeys (myKeys)
import XMonad.Config.MyXMobarPP (myXMobarPP)
import XMonad.Config.Scratchpads (myScratchpads)
import XMonad.Config.Theme (theme, myTabTheme)

import XMonad.Hooks.DynamicLog (dynamicLogString, xmonadPropLog)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)

import XMonad.Layout (Tall(..))
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Constrained (Constrained(..))
import XMonad.Layout.IfMax (IfMax(..))
import XMonad.Layout.Maximize (maximizeWithPadding)
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Tabbed (addTabs, shrinkText)
import XMonad.Layout.WindowNavigation (windowNavigation)

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

main :: IO ()
main = xmonad $ docks $ ewmh $ myConfig

myConfig = theme { keys       = myKeys
                 , logHook    = dynamicLogString myXMobarPP >>= xmonadPropLog
                 , layoutHook = myLayout
                 , manageHook = namedScratchpadManageHook myScratchpads
                 , workspaces = myWorkspaces
                 }
           `additionalKeysP` (switchWorkspaceKeys)

myWorkspaces :: [String]
myWorkspaces = ["dev", "web", "chat", "x", "y", "config", "me", "music"]

qwertyHomeRow :: [String]
qwertyHomeRow = ["a", "s", "d", "f", "g", "j", "k", "l"]

colemakHomeRow :: [String]
colemakHomeRow = ["a", "r", "s", "t", "g", "n", "e", "i"]

homeRow :: [String]
homeRow = colemakHomeRow

switchWorkspaceKeys :: [(String, X ())]
switchWorkspaceKeys = [(maybeShift ++ "M4-" ++ key, action tag)
                      | (tag, key) <- zip myWorkspaces homeRow
                      , (maybeShift, action) <- [("", windows . W.greedyView)
                                                ,("S-", windows . W.shift)]]


myLayout = windowNavigation $ avoidStruts $ maximizeWithPadding 0 $ spaceWindows $ mySubTabbed $ boringWindows $
           constrainSingle (Tall 1 (1/100) (1/2))
           ||| constrainSingle (Tall 1 (1/100) (3/4))
           ||| constrainSingle (Tall 1 (1/100) (1/4))
  where
    constrainSingle = IfMax 1 Constrained
    spaceWindows = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True
    mySubTabbed x = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
