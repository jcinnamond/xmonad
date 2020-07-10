module Main (main) where

import XMonad (xmonad, def, logHook, layoutHook, workspaces, spawn, terminal, windows)
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog (dynamicLogString, xmobarPP, xmonadPropLog)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)

import XMonad.Layout (Tall(..))

import XMonad.Util.EZConfig (additionalKeysP)

main :: IO ()
main = do
  xmonad $ docks $ myConfig

myConfig = def { terminal = "termite"
               , logHook = dynamicLogString xmobarPP >>= xmonadPropLog
               , layoutHook = avoidStruts $ Tall 1 (1/100) (1/2)
               , workspaces = myWorkspaces
               }
           `additionalKeysP` switchWorkspaceKeys


myWorkspaces :: [String]
myWorkspaces = ["dev", "web", "chat", "x", "y", "config", "me", "music"]

homeRow :: [String]
homeRow = ["a", "s", "d", "f", "g", "j", "k", "l"]

switchWorkspaceKeys = [(maybeShift ++ "M4-" ++ key, action tag)
                      | (tag, key) <- zip myWorkspaces homeRow
                      , (maybeShift, action) <- [("", windows . W.greedyView)
                                                ,("S-", windows . W.shift)]]
