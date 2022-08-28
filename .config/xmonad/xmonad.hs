import XMonad

import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( isDialog )
import XMonad.Hooks.StatusBar ( withEasySB, statusBarProp, defToggleStrutsKey )
import XMonad.Hooks.StatusBar.PP ( PP )
-- import XMonad.Util.ClickableWorkspaces ( clickablePP )

import XMonad.Util.EZConfig ( additionalKeysP )
import XMonad.Util.Loggers ( logTitles )
import XMonad.Util.Ungrab ( unGrab )
-- import Xmonad.Util.NamedActions ( addDescrKeys, sendMessage' )

import XMonad.Layout.Magnifier ( magnifiercz' )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.Renamed ( renamed, Rename(Replace) )

import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )


main :: IO ()
main = xmonad
     . ewmhFullscreen -- make fullscreened applications work properly
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc1" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , terminal   = myTerminal    -- Use alacritty instead of xterm
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "slock"                        )
    , ("M-e"  , spawn $ myTerminal
                ++ " -t nnn -e nnn"                  )
    , ("M-C-e", spawn "dolphin"                      )
    , ("M1-e" , spawn myEditor                       )
    , ("M1-d" , spawn $ myEditor
                ++ " --eval '(emacs-everywhere)'"    )
    , ("M1-d" , spawn $ myEditor
                ++ " --eval '(dired nil)'"           )
    , ("M1-m" , spawn $ myEditor
                ++ " --eval '(mu4e)'"                )
    , ("M-w"  , spawn $ "feh --randomize --bg-fill "
                ++ "~/.local/share/wallpapers/*"     )
    , ("<Print>", unGrab *> spawn "maimpick"         )
    , ("<XF86Calculator>", spawn "qalculate-gtk"     )
    , ("<XF86AudioMute>", spawn "pamixer -t"         )
    , ("<XF86AudioLowerVolume>", spawn "pamixer --allow-boost -d 3")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer --allow-boost -i 3")
    , ("<XF86AudioMicMute>", spawn "pamixer --source 52 -t"        ) -- find source with `pamixer --list-sources`
    ]
 

myTerminal :: String
myTerminal = "alacritty"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs'"

-- use xprop to WM_CLASS, whose 1st value is the instance name
-- and matched-on via appName and the 2nd one is the class name
-- and can be accessed with className
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"          --> doFloat
    , className =? "Qalculate-gtk" --> doFloat
    , isDialog                     --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol
      = renamed [Replace "ThreeCol"]
      $ magnifiercz' 1.3
      $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
