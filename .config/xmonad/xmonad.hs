-- References: https://wiki.archlinux.org/title/Xmonad
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( isDialog )
import XMonad.Hooks.StatusBar ( withEasySB, statusBarProp, defToggleStrutsKey )
import XMonad.Hooks.StatusBar.PP ( PP )
import XMonad.Util.ClickableWorkspaces ( clickablePP )

import XMonad.Util.EZConfig ( additionalKeysP, checkKeymap )
import XMonad.Util.Loggers ( logTitles )
import XMonad.Util.Ungrab ( unGrab )
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Cursor (setDefaultCursor, xC_pirate)
-- import Xmonad.Util.NamedActions ( addDescrKeys, sendMessage' )

import XMonad.Layout.Magnifier ( magnifiercz' )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.Renamed ( renamed, Rename(Replace) )
import XMonad.Layout.Grid

import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Actions.CycleWS ( prevWS, nextWS, shiftToPrev, shiftToNext )
import XMonad.Operations ( kill )
-- XMonad.Operations save/restore state

import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import qualified XMonad.Prompt (def)
import System.Exit (exitSuccess)

import XMonad.Hooks.ScreenCorners

main :: IO ()
main = xmonad $ ewmhFullscreen . ewmh
     $ withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobar.hs" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig

-- Default keybindings: "M-S-/"
myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = screenCornerLayoutHook $ myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , terminal   = myTerminal    -- Use myTerminal instead of xterm
    , handleEventHook = handleEventHook def <+> screenCornerEventHook
    , startupHook = myStartupHook
    }
  `additionalKeysP` myKeymap

myKeymap =
    [ ("M-S-z", spawn "slock"                                          )
    , ("M-q"  , spawn "xmonad --recompile && xmonad --restart"         )
    , ("M-S-q", confirmPrompt XMonad.Prompt.def "Exit?" $ io exitSuccess)
    , ("M-x"  , kill                                                   )
    , ("M-S-x", spawn "xkill"                                          )
    , ("M-d"  , spawn "~/.config/rofi/scripts/launcher_t7"             )

    , ("M-n"  , nextWS                                                 )
    , ("M-p"  , prevWS                                                 )
    , ("M-S-n", shiftToNext                                            )
    , ("M-S-p", shiftToPrev                                            )

    , ("M-C-e"  , spawn $ myTerminal
                ++ " -t nnn -e nnn -e"                                 )
    , ("M-e", spawn "nautilus"                                         )
    -- , ("M-p"  , spawn "rofi -show drun -show-icons -display-drun ''")
    -- , ("M-p", spawn "~/.config/rofi/scripts/launcher_t7"               )

    , ("M1-a" , spawn $ "emacsclient --eval '(emacs-everywhere         )'" )
    , ("M1-b" , spawn "qutebrowser"                                    )
    , ("M1-e" , spawn myEditor                                         )
    , ("M1-t" , spawn $ myTerminal
                ++ " -t ec -e emacsclient -s term -nw -c"              )
    , ("M1-d" , spawn $ myEditor
                ++ " --eval '(dired nil                                )'" )
    , ("M1-m" , spawn $ myEditor
                ++ " --eval '(mu4e                                     )'" )

    , ("M-w"  , spawn $ "feh --randomize --bg-fill "
                ++ "~/.local/share/wallpapers/*"                    )
    , ("<Print>", unGrab *> spawn "maimpick select-copy"            )
    , ("M-<Print>", unGrab *> spawn "maimpick"                      )
    , ("M-<Page_Up>", spawn "kbd-backlight up"                      )
    , ("M-<Page_Down>", spawn "kbd-backlight down"                  )
    -- , ("M-<Backspace>", spawn "sysact"                           )
    , ("M-<Backspace>", spawn "~/.config/rofi/scripts/powermenu_t3" )
    , ("<XF86Favorites>", spawn "~/.config/rofi/scripts/launcher_t7")
    , ("<XF86Display>", spawn "arandr"                              )
    , ("<XF86MonBrightnessUp>"  , spawn $ myScriptsPrefix ++ "brightness.sh up"  )
    , ("<XF86MonBrightnessDown>", spawn $ myScriptsPrefix ++ "brightness.sh down")
    , ("<XF86Calculator>", spawn "gnome-calculator"                 ) -- qalculate-gtk

    -- Compile pamixer from source (github: cdemoulins/pamixer)
    , ("<XF86AudioMute>"       , spawn $ myScriptsPrefix ++ "volume.sh mute"      )
    , ("<XF86AudioLowerVolume>", spawn $ myScriptsPrefix ++ "volume.sh down"      )
    , ("<XF86AudioRaiseVolume>", spawn $ myScriptsPrefix ++ "volume.sh up"        )
    , ("<XF86AudioMicMute>"    , spawn $ myScriptsPrefix ++ "volume.sh mute-mic"  )

    -- Temporary
    , ("M-<F1>", spawn "sxiv -r -q -t -o /home/lokesh/.local/share/wallpapers/*")
    , ("M1-0", spawn "picom-trans 100") -- transset 1.00
    , ("M1-1", spawn "picom-trans 90")  -- transset 0.90
    , ("M1-2", spawn "picom-trans 85")  -- transset 0.85
    , ("M1-3", spawn "picom-trans 80")  -- transset 0.80
    , ("M1-4", spawn "picom-trans 75")  -- transset 0.75
    ]


myTerminal :: String
myTerminal = "alacritty"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs'"

myScriptsPrefix :: String
myScriptsPrefix = "/home/lokesh/.config/xmonad/scripts/"

-- use xprop to WM_CLASS, whose 1st value is the instance name
-- and matched-on via appName and the 2nd one is the class name
-- and can be accessed with className
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"          --> doFloat
    , className =? "Qalculate-gtk" --> doFloat
    , className =? "Arandr"        --> doFloat
    , className =? "Pavucontrol"   --> doFloat
    , isDialog                     --> doFloat
    , className =? "Google-chrome" --> doShift "2"
    , className =? "Microsoft Teams - Preview" --> doShift "9"
    , appName =? "Communication" --> doShift "8"
    , className =? "Thunderbird" --> doShift "8"
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| Grid ||| threeCol
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

myStartupHook :: X()
myStartupHook = do
  -- spawnOnce "dunst"
  spawnOnce "lxpolkit &" -- polkit authentication agent (https://wiki.archlinux.org/title/Polkit#Authentication_agents)
  spawnOnce "/usr/bin/picom &"
  spawnOnce "/home/lokesh/.local/bin/custom/remaps"
  spawnOnce "feh --randomize --bg-fill ~/.local/share/wallpapers/*"
  spawnOnce " trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 55 --tint 0x282c34 --height 25 &"
  spawnOnce "nm-applet &"
  setDefaultCursor xC_left_ptr

  -- Check duplicate keybindings
  return () >> checkKeymap myConfig myKeymap

  -- Add hot corners
  addScreenCorners [ (SCLowerLeft,  prevWS)
                   , (SCLowerRight, nextWS)
                   -- , (SCUpperLeft, spawnSelected' myAppGrid)
                   -- , (SCUpperRight, goToSelected $ mygridConfig' myColorizer)
                   ]
  setWMName "LG3D"
