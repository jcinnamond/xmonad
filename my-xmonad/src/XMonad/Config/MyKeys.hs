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
import Graphics.X11.Xlib
    ( controlMask,
      mod4Mask,
      noModMask,
      shiftMask,
      xK_Escape,
      xK_Left,
      xK_Return,
      xK_Right,
      xK_Tab,
      xK_comma,
      xK_equal,
      xK_j,
      xK_k,
      xK_l,
      xK_m,
      xK_n,
      xK_period,
      xK_q,
      xK_s,
      xK_space,
      xK_z,
      stringToKeysym,
      KeyMask,
      KeySym ) -- used for masks, keys, and `stringToKeysym` for looking up multimedia keys
import System.Exit ( ExitCode(ExitSuccess), exitWith )

import XMonad (ChangeLayout(NextLayout), IncMasterN(..), io, kill, sendMessage, spawn, windows, withFocused, XConfig, X ())
import qualified XMonad.StackSet as W

import XMonad.Config.Theme (myXP)
import XMonad.Config.Scratchpads (myScratchpads)

import XMonad.Layout.BoringWindows (focusDown, focusMaster, focusUp)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.SubLayouts (onGroup, pullGroup, GroupMsg(UnMerge))
import XMonad.Layout.WindowNavigation (Direction2D(U,D))

import XMonad.Prompt (autoComplete, searchPredicate, sorter, XPConfig)
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
    * Super-Control-Space  Change layout
    * Super-Control-j      Merge with window/group below
    * Super-Control-k      Merge with window/group above
    * Super-Control-n      Unmerge window from group
    * Super-m              Maximize window

  == Control XMonad
    * Super-q        Restart XMonad
    * Super-Shift-q  Quit XMonad

  == Launchers and running commands
    * Super-<Return>           Pop up terminal
    * Super-Shift-<Return>     Pull terminal into layout
    * Super-<Space>            Command launcher
    * Super-Control-<Return>   Prompt for XMonad actions
    * Super-z                  Suspend
    * Super-Control-m          Fix monitors
    * Super-l                  Lock screen

  It also adds some multimedia key bindings.
-}
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys _ = M.fromList $ [
  -- Manage windows
    ((mod4Mask, xK_Tab), focusDown)
  , ((mod4Mask .|. controlMask, xK_Tab), onGroup W.focusDown')
  , ((mod4Mask .|. shiftMask, xK_Tab), focusUp)
  , ((mod4Mask .|. controlMask .|. shiftMask, xK_Tab), onGroup W.focusUp')
  , ((mod4Mask, xK_Escape), focusMaster)
  , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
  , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
  , ((mod4Mask, xK_Left), windows W.swapDown)
  , ((mod4Mask, xK_Right), windows W.swapUp)
  , ((mod4Mask, xK_equal), windows W.swapMaster)
  , ((mod4Mask .|. shiftMask, xK_k), kill)
  , ((mod4Mask .|. shiftMask, xK_s), withFocused $ windows . W.sink)

  -- Manage layouts
  , ((mod4Mask .|. controlMask, xK_space), sendMessage NextLayout)
  , ((mod4Mask .|. controlMask, xK_j), sendMessage $ pullGroup D)
  , ((mod4Mask .|. controlMask, xK_k), sendMessage $ pullGroup U)
  , ((mod4Mask .|. controlMask, xK_n), withFocused (sendMessage . UnMerge))
  , ((mod4Mask, xK_m), withFocused (sendMessage . maximizeRestore))

  -- Control XMonad
  , ((mod4Mask, xK_q), spawn "xmonad --recompile && xmonad --restart")
  , ((mod4Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

  -- Launchers and running commands
  , ((mod4Mask, xK_Return), namedScratchpadAction myScratchpads "popTerm")
  , ((mod4Mask .|. shiftMask, xK_Return), namedScratchpadAction myScratchpads "pullTerm")
  , ((mod4Mask, xK_space), shellPrompt myPromptConfig)
  , ((mod4Mask .|. controlMask, xK_Return), xmonadPrompt myPromptConfig)
  , ((mod4Mask, xK_z), spawn "systemctl suspend")
  , ((mod4Mask .|. controlMask, xK_m), spawn "~/bin/monitors")
  , ((mod4Mask, xK_l), spawn "xautolock -locknow")

   -- Multimedia keys
  , ((noModMask, stringToKeysym "XF86AudioPlay"), spawn "playerctl play-pause")
  , ((noModMask, stringToKeysym "XF86AudioRaiseVolume"), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ((noModMask, stringToKeysym "XF86AudioLowerVolume"), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ((noModMask, stringToKeysym "XF86AudioMute"), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  ]
