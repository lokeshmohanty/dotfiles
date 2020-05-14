import System.IO (hPutStrLn)
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Hooks.SetWMName

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layouts
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenEventHook)
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Decoration (def)

-- For transparent Hook
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Data.Monoid
import Data.Word (Word32)

-- Scratch Pad (--contrib)
-- import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook, NamedScratchpad(NS), customFloating, namedScratchpadAction)

------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "st"

-- Location of your xmobar.hs / xmobarrc
myXmobarrc = "~/.xmonad/xmobarrc"

-- Custom Functions

myUnion :: Ord k => (t -> M.Map k a) -> (t -> M.Map k a) -> (t -> M.Map k a)
myUnion y z = \x -> M.union (y x) (z x)

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1: TERM","2: WEB","3: CODE","4: OTHER"] ++ map show [5..9]

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "2:WEB"
    , className =? "Firefox"        --> doShift "2:WEB"
    , className =? "Google-chrome"  --> doShift "2:WEB"
    , className =? "Code - OSS"     --> doShift "3:CODE"
    , className =? "PostmanCanary"  --> doShift "4:OTHER"
    -- Java because of dbeaver
    , className =? "Java"           --> doShift "4:OTHER"
    , className =? "DBeaver"        --> doShift "4:OTHER"

    , className =? "ringcentral"    --> doShift "7"

    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    -- , className =? "MPlayer"        --> doCenterFloat
    , className =? "feh"            --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

-- manageScratchPad :: ManageHook
-- manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
--   where h = 0.1     -- terminal height, 10%
--         w = 1       -- terminal width, 100%
--         t = 1 - h   -- distance from top edge, 90%
--         l = 1 - w   -- distance from left edge, 0%
--
-- scratchPad = scratchpadSpawnActionTerminal "urxvt"

myScratchPads = [ NS "terminal" spawnTerm  findTerm  manageTerm 
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"              -- tilte can be set instead of name, keep the syntax in mind
    findTerm   = resource  =? "scratchpad"                   -- use title if title is set
    manageTerm = customFloating $ W.RationalRect 0 0.9 1 0.1 -- position and size

scratchTerm = namedScratchpadAction myScratchPads "terminal"

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) 
    ||| noBorders (fullscreenFull Full)
    ||| Mirror (Tall 1 (3/100) (1/2)) 
    ||| tabbed shrinkText tabConfig 
    ||| Full
    )

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def
    { activeBorderColor     = "#7C7C7C"
    , activeTextColor       = "#CEFFAC"
    , activeColor           = "#000000"
    , inactiveBorderColor   = "#7C7C7C"
    , inactiveTextColor     = "#EEEEEE"
    , inactiveColor         = "#000000"
    }

-- Color of current window title in xmobar.
-- xmobarTitleColor = "#FFB6B0"
xmobarTitleColor = "#C678DD"

-- Color of current workspace in xmobar.
-- xmobarCurrentWorkspaceColor = "#CEFFAC"
xmobarCurrentWorkspaceColor = "#51AFEF"

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask

lockScreen = "slimlock"
launcher   = "dmenu_run -l 15"
selectScreenshot = "sleep 0.5s; scrot -sf -e 'mv $f ~/Pictures/ && xdg-open ~/Pictures/$f'"
lowQualityScreenshot = "sleep 0.5s; scrot -sf -e 'mv $f ~/Pictures/ && xdg-open ~/Pictures/$f'' --quality 100"
windowScreenshot = "sleep 0.5s; scrot -sf -e 'mv $f ~/Pictures/ && xdg-open ~/Pictures/$f'' --quality 10"
screenshot = "scrot -e 'mv $f ~/Pictures/ && xdg-open ~/Pictures/$f'"

muteVolume = "amixer -q set Master toggle"
lowerVolume = "amixer -q set Master 5%-"
raiseVolume = "amixer -q set Master 5%+"

increaseBrightness = "xbacklight -inc 5"
decreaseBrightness = "xbacklight -dec 5"

play = "playerctl play-pause"
stop = "playerctl next"
next = "playerctl previous"
prev = "playerctl stop"


----------------------------------------------------------------------
-- Custom key bindings
--

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, xK_0), spawn lockScreen)
  , ((modMask, xK_p), spawn launcher)
  , ((modMask .|. shiftMask, xK_t), scratchTerm)

  , ((0, xK_Print), spawn selectScreenshot)
  , ((modMask , xK_Print), spawn screenshot)
  , ((modMask .|. shiftMask, xK_Print), spawn lowQualityScreenshot)
  , ((modMask .|. altMask, xK_Print), spawn windowScreenshot)

  , ((0, xF86XK_AudioMute), spawn muteVolume)
  , ((0, xF86XK_AudioLowerVolume), spawn lowerVolume)
  , ((0, xF86XK_AudioRaiseVolume), spawn raiseVolume)

  , ((0, xF86XK_MonBrightnessUp),  spawn increaseBrightness)
  , ((0, xF86XK_MonBrightnessDown), spawn decreaseBrightness)

  , ((0, xF86XK_AudioPlay), spawn play)
  , ((0, xF86XK_AudioNext), spawn next)
  , ((0, xF86XK_AudioPrev), spawn prev)
  , ((0, xF86XK_AudioStop), spawn stop)
  
  -- Recompile and Restart xmonad without killing any process
  , ((modMask .|. altMask, xK_q), spawn "xmonad --recompile && xmonad --restart")

  -- Forcefully kill an application
  , ((modMask, xK_Escape), spawn "xkill" )

  -- Eject CD tray.
  -- , ((0, 0x1008FF2C), spawn "eject -T")

  ]

------------------------------------------------------------------------
-- setTransparentHook
--

setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
    opacityFloat = 0.9
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)
--

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
myStartupHook = do
  setWMName "LG3D"
  -- spawn     "bash ~/.xmonad/startup.sh"
  spawn "picom -b -d :0"
  spawn "setxkbmap -option caps:swapescape"
  setDefaultCursor xC_left_ptr

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 70
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
  }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
--
defaults = def 
    { terminal           = myTerminal
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myUnion myKeys $ keys def
    , layoutHook         = smartBorders $ myLayout
    , manageHook         = manageDocks <+> myManageHook <+> namedScratchpadManageHook myScratchPads
    , handleEventHook    = docksEventHook <+> setTransparentHook
    , startupHook        = myStartupHook
}
