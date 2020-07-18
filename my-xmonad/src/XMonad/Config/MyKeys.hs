{-# LANGUAGE NumericUnderscores #-}

{-|
  Module       : XMonad.Config.MyConfig
  Copyright    : (c) 2010 John Cinnamond (john@cinnamond.me.uk)
  License      : BSD

  This module defines the keybindings I like to use with XMonad.
-}
module XMonad.Config.MyKeys (
  myKeys
  ) where

import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.Xlib -- used for masks, keys, and `stringToKeysym` for looking up multimedia keys
import System.Exit

import XMonad (ChangeLayout(NextLayout), IncMasterN(..), io, kill, sendMessage, spawn, windows, withFocused, XConfig, X ())
import qualified XMonad.StackSet as W

import XMonad.Config.Theme (myXP)
import XMonad.Config.Scratchpads (myScratchpads)

import XMonad.Layout.BoringWindows (focusDown, focusMaster, focusUp)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.SubLayouts (pullGroup, GroupMsg(UnMerge))
import XMonad.Layout.WindowNavigation (Direction2D(U,D))

import XMonad.Prompt (autoComplete, def, searchPredicate, sorter, XPConfig)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)

import XMonad.Util.NamedScratchpad (namedScratchpadAction)

myPromptConfig :: XPConfig
myPromptConfig = myXP { autoComplete = Just 200_000
                      , searchPredicate = fuzzyMatch
                      , sorter          = fuzzySort
                      }


{-|
  myKeys defines all of the keybindings I use for XMonad.
  It is designed to completely replace the default keybindings,
  rather than augment them. To use it, add something like the
  following to your XMonad config:

  > import XMonad.Config.MyConfig (myKeys)
  > main = xmonad def { keys = myKeys }

  The keybindings are all based of super/win/cmd (i.e., mod4) apart
  from some special multimedia keys. They shouldn't clash with other
  applications, in particular emacs which has a lot of bindings that
  use Meta. The keybindings are designed to avoid the home row for
  both qwerty and colemak layouts as I use those for changing
  workspaces.

  The key bindings are as follows:

  == Manage windows
    * Super-Tab                Focus next gropu
    * Super-Control-Tab        Focus next window (including in group)
    * Super-Shift-Tab          Focus previous group
    * Super-Control-Shift-Tab  Focus previous window (including in group)
    * Super-Escape             Focus primary group
    * Super-,                  Pull first secondary window to primary area
    * Super-.                  Push last primary window to secondary area
    * Super-=                  Make current window master
    * Super-Left               Swap focused and previous window
    * Super-Right              Swap focused and next window
    * Super-Shift-k            Close focused window
    * Super-Shift-s            Sink floating window

  == Manage layouts
    * Super-Control-j  Merge with window/group below
    * Super-Control-k  Merge with window/group above
    * Super-Control-n  Unmerge window from group
    * Super-m          Maximize window

  == Control XMonad
    * Super-Shift-Space   Change layout
    * Super-q             Restart XMonad
    * Super-Shift-q       Quit XMonad

  == Launchers and running commands
    * Super-<Space>            Pop up terminal
    * Super-<Return>           Command launcher
    * Super-Control-<Return>   Prompt for XMonad actions
    * Super-z                  Suspend

  It also adds some multimedia key bindings.
-}
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys _ = M.fromList $ [
  -- Manage windows
    ((mod4Mask, xK_Tab), focusDown)
  , ((mod4Mask .|. controlMask, xK_Tab), windows W.focusDown)
  , ((mod4Mask .|. shiftMask, xK_Tab), focusUp)
  , ((mod4Mask .|. controlMask .|. shiftMask, xK_Tab), windows W.focusUp)
  , ((mod4Mask, xK_Escape), focusMaster)
  , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
  , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
  , ((mod4Mask, xK_Left), windows W.swapDown)
  , ((mod4Mask, xK_Right), windows W.swapUp)
  , ((mod4Mask, xK_equal), windows W.swapMaster)
  , ((mod4Mask .|. shiftMask, xK_k), kill)
  , ((mod4Mask .|. shiftMask, xK_s), withFocused $ windows . W.sink)

  -- Manage layouts
  , (((mod4Mask .|. controlMask, xK_j), sendMessage $ pullGroup D))
  , (((mod4Mask .|. controlMask, xK_k), sendMessage $ pullGroup U))
  , (((mod4Mask .|. controlMask, xK_n), withFocused (sendMessage . UnMerge)))
  , (((mod4Mask, xK_m), withFocused (sendMessage . maximizeRestore)))

  -- Control XMonad
  , ((mod4Mask .|. shiftMask, xK_space), sendMessage NextLayout)
  , ((mod4Mask, xK_q), spawn "xmonad --recompile && xmonad --restart")
  , ((mod4Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

  -- Launchers and running commands
  , ((mod4Mask, xK_space), namedScratchpadAction myScratchpads "popTerm")
  , ((mod4Mask, xK_Return), shellPrompt myPromptConfig)
  , ((mod4Mask .|. controlMask, xK_Return), xmonadPrompt myPromptConfig)
  , ((mod4Mask, xK_z), spawn "systemctl suspend")

   -- Multimedia keys
  , ((noModMask, stringToKeysym "XF86AudioPlay"), spawn "playerctl play-pause")
  , ((noModMask, stringToKeysym "XF86AudioRaiseVolume"), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ((noModMask, stringToKeysym "XF86AudioLowerVolume"), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ((noModMask, stringToKeysym "XF86AudioMute"), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  ]
