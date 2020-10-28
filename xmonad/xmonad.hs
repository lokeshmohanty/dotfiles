import System.IO (hPutStrLn)
import System.Exit
import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Hooks.SetWMName

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

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)

-- Scratch Pad (--contrib)
-- import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook, NamedScratchpad(NS), customFloating, namedScratchpadAction)

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Pass (passPrompt, passGeneratePrompt, passRemovePrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
-- import XMonad.Prompt.DirExec (dirExecPrompt)

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
    , className =? "google-chrome"  --> doShift "2:WEB"
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
    -- , className =? "URxvt" <&&> title =? "neomutt" --> doShift "net"
    , className =? "feh"            --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , className =? "stalonetray"    --> doIgnore
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myScratchPads = [ NS "terminal" spawnTerm  findTerm  manageTerm   -- scratchpad
                , NS "email"     spawnEmail  findEmail  manageEmail 
                , NS "irc"     spawnIrc  findIrc  manageIrc   
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"              -- tilte can be set instead of name, keep the syntax in mind
    findTerm   = resource   =? "scratchpad"                   -- use title if title is set
    manageTerm = customFloating $ W.RationalRect 0 0.9 1 0.1 -- position and size

    spawnEmail  = "xfce4-terminal" ++ " -T email -e aerc"
    findEmail   = title   =? "email"
    manageEmail = customFloating $ W.RationalRect 0.1 0.1 0.8 0.8

    spawnIrc  = "xfce4-terminal" ++ " -T irc -e weechat"
    findIrc   = title   =? "irc"
    manageIrc = customFloating $ W.RationalRect 0.1 0.1 0.8 0.8

scratchTerm  = namedScratchpadAction myScratchPads "terminal"
scratchEmail = namedScratchpadAction myScratchPads "email"
scratchIrc   = namedScratchpadAction myScratchPads "irc"

-- My Xmonad Prompt Config
myXPConfig = def
  { font        = "xft:Source Code Pro:pixelsize=12"
  , borderColor = "#1e2320"
  , fgColor     = "#dddddd"
  , fgHLight    = "#ffffff"
  , bgColor     = "#1e2320"
  , bgHLight    = "#5f5f5f"
  , height      = 18
  , position    = Top
  }

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
launchShell = shellPrompt myXPConfig
launchPrompt = runOrRaisePrompt myXPConfig
selectScreenshot = "sleep 0.5s; scrot -sf -e 'mv $f ~/Pictures/ && xdg-open ~/Pictures/$f'"
copyScreenshot = "sleep 0.5s; scrot '/tmp/%F_%T_$wx$h.png' -se 'xclip -selection clipboard -target image/png -i $f'"
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
  -- , ((modMask .|. shiftMask, xK_p), spawn launcher)
  , ((modMask .|. shiftMask, xK_p), launchShell)
  , ((modMask, xK_p), launchPrompt)

  , ((modMask .|. altMask                   , xK_p), passPrompt myXPConfig)
  , ((modMask .|. controlMask               , xK_p), passGeneratePrompt myXPConfig)
  , ((modMask .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt myXPConfig)

  , ((modMask .|. shiftMask, xK_t), scratchTerm)
  , ((modMask .|. shiftMask, xK_e), scratchEmail)
  , ((modMask .|. shiftMask, xK_i), scratchIrc)

  , ((0, xK_Print), spawn copyScreenshot)
  , ((modMask , xK_Print), spawn screenshot)
  , ((modMask .|. controlMask, xK_Print), spawn selectScreenshot)
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
  -- spawn "picom -b -d :0"
  spawn "picom -bcCGf -i 0.8 -e 0.8 --no-fading-openclose --sw-opti"
  spawn "setxkbmap -option caps:swapescape"
  setDefaultCursor xC_left_ptr
  spawn "dunst"

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
