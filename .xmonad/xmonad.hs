import XMonad
import qualified XMonad.StackSet as W

-- Data
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

-- Utils
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch
import Control.Arrow (first)

-- Layout
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace (onWorkspace)
 
-- Hooks
import XMonad.Hooks.WorkspaceHistory

---
-- Variables
---
myFont :: String
myFont        = "xft:JetBrainsMono Nerd Font:size=10"

myTerminal :: String
myTerminal    = "alacritty"

myModMask :: KeyMask
myModMask     = mod4Mask -- Win key

altMask :: KeyMask
altMask       = mod1Mask -- Used by xprompts for alt

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor   = "#6d7d7a"

myFocusColor :: String
myFocusColor  = "#aec7c2"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


---
-- Additional Keys
---
-- Because of XMonad.Util.EZConfig, keymaps can be written simpler like this
myKeys :: [(String, X ())]
myKeys =
    [ ("M-S-<Return>", shellPrompt myXPromptConfig)
    , ("M-<Return>", spawn myTerminal)
    ]

---
-- Prompt Config
---
myXPromptConfig :: XPConfig
myXPromptConfig =
    def
      { font                = myFont
      , bgColor             = "#303533"
      , fgColor             = "#c0c2c1"
      , bgHLight            = "#444947"
      , fgHLight            = "#d5d6d6"
      , borderColor         = myNormColor
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      , position            = Top
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing
      }

--myPromptList :: [(String, XPConfig -> X ())]
--promptList = []

---
-- XPrompt Keymap
---
myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
         [ (xK_z, killBefore)            -- kill line backwards
         , (xK_k, killAfter)             -- kill line forwards
         , (xK_a, startOfLine)           -- move to the beginning of the line
         , (xK_e, endOfLine)             -- move to the end of the line
         , (xK_m, deleteString Next)     -- delete a character foward
         , (xK_b, moveCursor Next)       -- move cursor forward
         , (xK_f, moveCursor Prev)       -- move cursor backward
         , (xK_BackSpace, killWord Prev) -- kill the previous word
         , (xK_y, pasteString)           -- paste a string
         , (xK_g, quit)                  -- quit out of prompt
         , (xK_bracketleft, quit)
         ]
         ++
         map (first $ (,) altMask)       -- meta key + <key>
         [ (xK_BackSpace, killWord Prev) -- kill the prev word
         , (xK_f, moveWord Next)         -- move a word forward
         , (xK_b, moveWord Prev)         -- move a word backward
         , (xK_d, killWord Next)         -- kill the next word
         , (xK_n, moveHistory W.focusUp')   -- move up thru history
         , (xK_p, moveHistory W.focusDown') -- move down thru history
         ]
         ++
         map (first $ (,) 0) -- <key>
         [ (xK_Return, setSuccess True >> setDone True)
         , (xK_KP_Enter, setSuccess True >> setDone True)
         , (xK_BackSpace, deleteString Prev)
         , (xK_Delete, deleteString Next)
         , (xK_Left, moveCursor Prev)
         , (xK_Right, moveCursor Next)
         , (xK_Home, startOfLine)
         , (xK_End, endOfLine)
         , (xK_Down, moveHistory W.focusUp')
         , (xK_Up, moveHistory W.focusDown')
         , (xK_Escape, quit)
         ]

---
-- Startup Hooks
---
myStartupHook :: X()
myStartupHook = do
    spawnOnce "~/.fehbg &"
    spawnOnce "picom &"
    setWMName "LG3D"

---
-- Workspaces
---
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev", "www", "sys", "chat", "doc","game"]

---
-- Layouts
---
defaultLayout = layoutHook def
myLayouts     = avoidStruts $ onWorkspace "game" simpleFloat $ defaultLayout

---
-- Main
---

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar/.xmobarrc"
  xmonad $ docks def
    { terminal            = myTerminal
    , modMask             = myModMask
    , borderWidth         = myBorderWidth
    , normalBorderColor   = myNormColor
    , focusedBorderColor  = myFocusColor
    , layoutHook          = myLayouts
    , logHook             = workspaceHistoryHook <+> dynamicLogWithPP xmobarPP
                                { ppOutput          = hPutStrLn xmproc
                                , ppTitle           = xmobarColor "green" "" . shorten 50
                                , ppCurrent         = xmobarColor "#D9F7F0" "" . wrap "[" "]"
                                , ppVisible         = xmobarColor "#D5F2EB" ""
                                , ppHidden          = xmobarColor "#D5F2EB" "" . wrap "*" ""
                                , ppHiddenNoWindows = xmobarColor "#D5F2EB" ""
                                , ppSep             = "<fc=#666666> | </fc>"
                                , ppUrgent          = xmobarColor "#921414" "" . wrap "!" "!"
                                , ppExtras          = [windowCount]
                                , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                                }
    , startupHook         = myStartupHook
    , workspaces          = myWorkspaces
    } `additionalKeysP` myKeys
