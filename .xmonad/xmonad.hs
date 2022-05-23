{-# OPTIONS_GHC -Wno-deprecations #-} -- remove this to check deprecations
-- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen, nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

-- Hooks
-- import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, xmobarAction, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.XMonad
import Control.Arrow (first)

-- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask, altMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key
altMask = mod1Mask

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser

myEmacs, myOtherEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type
myOtherEmacs = "emacsclient -s other -c"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 1           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"   -- Border color of focused windows

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

setRandomWallpaper :: String
setRandomWallpaper = "feh --randomize --bg-fill ~/Pictures/Wallpapers/*"

myStartupHook :: X ()
myStartupHook = do
    -- spawnOnce "lxsession &"
    spawnOnce "picom &"
    spawnOnce "nm-applet &"
    spawnOnce "volumeicon &"
    spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 255 --tint 0x282c34  --height 22 &"
    -- spawnOnce "/usr/bin/emacs --daemon=other &" -- emacs daemon for other apps like chat and mail
    -- spawnOnce "emacs --daemon &" -- emacs daemon for the emacsclient, -> graphics not being rendered properly
    -- spawnOnce "~/.fehbg &"  -- set last saved feh wallpaper
    spawnOnce "feh --randomize --bg-fill ~/Pictures/Wallpapers/*"  -- feh set random wallpaper
    spawnOnce "redshift &"
    setWMName "LG3D"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Emacs", "emacs")
            , ("EmacsClient", "emacsclient -c -a emacs")
            , ("EmacsServer", "emacs --eval 'server-start'")
            , ("Alacritty", "alacritty")
            , ("EmacsServer(other)", "emacs --eval 'my/mu4e-server'")
            , ("EmacsClient(other)", "emacsclient -s other -c")
            , ("RingCentralMeetings", "ringcentral")
            , ("QuteBrowser", "qutebrowser")
            , ("Dbeaver", "dbeaver")
            , ("Firefox", "firefox")
            , ("Postman", "postman")
            , ("Calculator", "qalculate-gtk")
            , ("Brave Browser", "brave-browser")
            ]

-- -- Example XPrompt Config
-- myBackgroundColor = "#151515"

-- myContentColor = "#d0d0d0"

-- myFont = "xft:VictorMono Nerd Font:regular:pixelsize=13"

-- myXPromptConfig :: XPConfig
-- myXPromptConfig =
--   XPC
--     { promptBorderWidth = 1
--     , alwaysHighlight = True
--     , height = 22
--     , historySize = 256
--     , font = myFont
--     , bgColor = myBackgroundColor
--     , fgColor = myContentColor
--     , bgHLight = myBackgroundColor
--     , fgHLight = myContentColor
--     , borderColor = myBackgroundColor
--     , position = Top
--     , autoComplete = Nothing
--     , showCompletionOnTab = False
--     , searchPredicate = fuzzyMatch
--     , defaultPrompter = id
--     , sorter = const id
--     , maxComplRows = Just 7
--     , promptKeymap = defaultXPKeymap
--     , completionKey = (0, xK_Tab)
--     , changeModeKey = xK_grave
--     , historyFilter = id
--     , defaultText = []
--     }

dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Top
      -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 23
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { autoComplete        = Nothing
      }

emojiXPConfig :: XPConfig
emojiXPConfig = dtXPConfig
      { font             = myEmojiFont
      }

dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)      -- control + <key>
     [ (xK_z, killBefore)               -- kill line backwards
     , (xK_k, killAfter)                -- kill line forwards
     , (xK_a, startOfLine)              -- move to the beginning of the line
     , (xK_e, endOfLine)                -- move to the end of the line
     , (xK_m, deleteString Next)        -- delete a character foward
     , (xK_b, moveCursor Prev)          -- move cursor forward
     , (xK_f, moveCursor Next)          -- move cursor backward
     , (xK_BackSpace, killWord Prev)    -- kill the previous word
     , (xK_y, pasteString)              -- paste a string
     , (xK_g, quit)                     -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)          -- meta key + <key>
     [ (xK_BackSpace, killWord Prev)    -- kill the prev word
     , (xK_f, moveWord Next)            -- move a word forward
     , (xK_b, moveWord Prev)            -- move a word backward
     , (xK_d, killWord Next)            -- kill the next word
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

archwiki, ebay, news, reddit, urban, yacy :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="
yacy     = S.searchEngine "yacy" "http://localhost:8090/yacysearch.html?query="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("S-y", yacy)
             , ("z", S.amazon)
             ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad -e fish"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect 0 0.9 1 0.1 -- position and size
    -- manageTerm = customFloating $ W.RationalRect l t w h
    --            where
    --              h = 0.9
    --              w = 0.9
    --              t = 0.95 -h
    --              l = 0.95 -w
    spawnMocp  = "emacs" ++ " -t mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "tallAccordion"]
           $ Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           $ Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| tallAccordion
                                 ||| wideAccordion
                                 ||| Main.magnify
                                 ||| noBorders monocle
                                 ||| noBorders tabs
                                 ||| floats
                                 ||| grid
                                 ||| spirals
                                 ||| threeCol
                                 ||| threeRow


myWorkspaces :: [String]
myWorkspaces = map (wrap " " " ") ["main", "web", "code", "irc/mail", "media", "notes", "other", "vbox", "meet"]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

-- clickable :: [(WorkspaceId, Int)] -> WorkspaceId -> String
-- clickable ws w = fromMaybe w $ (\x -> xmobarAction ("xdotool key=super+" ++ show x) "1" w) <$> lookup w ws

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "mpv"       --> doShift ( myWorkspaces !! 4 )
     , className =? "Gimp"      --> doShift ( myWorkspaces !! 4 )
     , className =? "Gimp"      --> doFloat
     , className =? "ringcentral"         --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "brave-browser-beta"   --> doShift ( myWorkspaces !! 1 )
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]
  -- <+> namedScratchpadManageHook myScratchPads

screenShot = "scrot -s '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots/'"
clipboardScreenshot = "scrot -se 'xclip -selection clipboard -t image/png -i $f'"
screenlock = "i3lock -c 001a00" -- slock
  
-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    -- KB_GROUP Xmonad
        [ ("M-q", spawn "xmonad --recompile && xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad
        , ("M-S-/", spawn "~/.xmonad/xmonad_keys.sh")

    -- KB_GROUP Run Prompt
        -- , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu
        , ("M-p", shellPrompt dtXPConfig) -- Xmonad Shell Prompt
        , ("M-C-p", runOrRaisePrompt dtXPConfig) -- run a program, open a file, or raise an already running program
    -- Other Prompts
        , ("M-o m", manPrompt dtXPConfig)          -- manPrompt
        , ("M-o p", passPrompt dtXPConfig)         -- passPrompt
        , ("M-o g", passGeneratePrompt dtXPConfig) -- passGeneratePrompt
        , ("M-o r", passRemovePrompt dtXPConfig)   -- passRemovePrompt
        , ("M-o s", sshPrompt dtXPConfig)          -- sshPrompt
        -- , ("M-p-u", mkUnicodePrompt "xsel" ["-b"] "/home/dt/.xmonad/UnicodeData.txt" emojiXPConfig) -- unicodePrompt (for copying emojis)
        , ("M-o x", xmonadPrompt dtXPConfig)       -- xmonadPrompt

        -- , ("M-p c", spawn "dm-colpick")   -- pick color from our scheme
        -- , ("M-p q", spawn "dm-logout")    -- logout menu

    -- KB_GROUP Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

        -- , ("M-.", nextWS)
        -- , ("M-,", prevWS)
        -- , ("M-<Escape>-l", shiftToNext)
        -- , ("M-<Escape>-h", shiftToPrev)

    -- KB_GROUP Kill windows
        , ("M-x", kill1)     -- Kill the currently focused client
        , ("M-S-x", killAll)   -- Kill all windows on current workspace
        , ("M-M1-x", spawn "xkill") -- Select a window to kill forcefully

    -- KB_GROUP Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- KB_GROUP Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- -- KB_GROUP Grid Select (CTR-g followed by a key)
        , ("M-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("M-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("M-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- KB_GROUP Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        -- , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-M1-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-M1-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-M1-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- KB_GROUP Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- KB_GROUP Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-M1-h", sendMessage $ pullGroup L)
        , ("M-M1-l", sendMessage $ pullGroup R)
        , ("M-M1-k", sendMessage $ pullGroup U)
        , ("M-M1-j", sendMessage $ pullGroup D)
        , ("M-M1-m", withFocused (sendMessage . MergeAll))
        , ("M-M1-u", withFocused (sendMessage . UnMerge))
        , ("M-M1-/", withFocused (sendMessage . UnMergeAll))
        , ("M-M1-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-M1-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- KB_GROUP Scratchpads
    -- Toggle show/hide these programs.  They run on a hidden workspace.
    -- When you toggle them to show, it brings them to your current workspace.
    -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M-s m", namedScratchpadAction myScratchPads "terminal")
        -- , ("M-s m", namedScratchpadAction myScratchPads "mocp")
        -- , ("M-s c", namedScratchpadAction myScratchPads "calculator")

    -- KB_GROUP Set wallpaper
    -- Set wallpaper with either 'xwallwaper'. Type 'SUPER+F1' to launch sxiv in the
    -- wallpapers directory; then in sxiv, type 'M1-x x' to set the wallpaper that you
    -- choose.  Or, type 'SUPER+F2' to set a random wallpaper.
        , ("M-<F1>", spawn "sxiv -r -q -t -o /home/lokesh/Pictures/Wallpapers/*")
        , ("M-<F2>", spawn "find /home/lokesh/Pictures/Wallpapers// -type f | shuf -n 1 | xargs xwallpaper --stretch")

    -- KB_GROUP Apps
        , ("M-e", spawn "xdg-open ~/")                 -- start default file viewer
        , ("M-a", spawn "emacsclient --eval '(emacs-everywhere)'")  -- edit any text in emacs
        , ("M1-e e", spawn myEmacs)                    -- start emacs
        , ("M1-e b", spawn setRandomWallpaper)         -- set random background
        , ("M1-e m", spawn myOtherEmacs)               -- mu4e email
        , ("M1-e 0", spawn "picom-trans 100")
        , ("M1-e 1", spawn "picom-trans 95")
        , ("M1-e 2", spawn "picom-trans 90")
        , ("M1-e 3", spawn "picom-trans 85")
        , ("M1-e 4", spawn "picom-trans 80")

        -- , ("M1-e b", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        -- , ("M1-e i", spawn (myEmacs ++ ("--eval '(erc)'")))       -- erc irc client
        -- , ("M1-e n", spawn (myEmacs ++ ("--eval '(elfeed)'")))    -- elfeed rss
        -- , ("M1-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        -- , ("M1-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))) -- vterm if on Doom Emacs
        -- , ("M1-e w", spawn (myEmacs ++ ("--eval '(eww \"distrotube.com\")'"))) -- eww browser if on GNU Emacs
        -- , ("M1-e w", spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"distrotube.com\"))'"))) -- eww browser if on Doom Emacs

    -- KB_GROUP Function Keys
        -- TODO: write a script to show the current brightness
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
        , ("M-<XF86MonBrightnessUp>", spawn "brightnessctl set +1%")
        , ("M-<XF86MonBrightnessDown>", spawn "brightnessctl set 1%-")
        , ("<XF86AudioPlay>", spawn "mocp --play")
        , ("<XF86AudioPrev>", spawn "mocp --previous")
        , ("<XF86AudioNext>", spawn "mocp --next")
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "qutebrowser https://www.google.com/")
        -- , ("<XF86Search>", spawn "dm-websearch")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        , ("<XF86Eject>", spawn "toggleeject")
        -- , ("<Print>", spawn "dm-maim")
        , ("<Print>", spawn screenShot)
        , ("S-<Print>", spawn clipboardScreenshot)
        , ("M-S-l", spawn screenlock)
        ]
    -- Appending search engine prompts to keybindings list.
    -- Look at "search engines" section of this config for values for "k".
        ++ [("M-s " ++ k, S.promptSearch dtXPConfig' f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
-- END_KEYS

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
    xmonad $ ewmh def
        { manageHook         = manageDocks <+> myManageHook <+> namedScratchpadManageHook myScratchPads
        -- -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
        -- -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        -- -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
        -- , handleEventHook    = serverModeEventHookCmd
        --                        <+> serverModeEventHook
        --                        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
        --                        <+> docksEventHook
        , handleEventHook    = docksEventHook -- deprecated
                               -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                               -- This works perfect on SINGLE monitor systems. On multi-monitor systems,
                               -- it adds a border around the window if screen does not have focus. So, my solution
                               -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                               -- <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP
          -- To filter scratch pad workspace
          -- $ filterOutWsPP.scratchpadWorkspaceTag
          $ xmobarPP
              -- the following variables beginning with 'pp' are settings for xmobar.
              { ppOutput = \x -> hPutStrLn xmproc  x
              , ppCurrent = xmobarColor "#c792ea" "" . wrap "<box type=Bottom width=2 mb=2 color=#c792ea>" "</box>"         -- Current workspace
              , ppVisible = xmobarColor "#c792ea" "" -- . clickable
              -- , ppVisible = xmobarColor "#c792ea" "" . wrap "*" "" . clickable (zip myWorkspaces [1..]) --  . clickable              -- Visible but not current workspace
              , ppHidden = xmobarColor "#82AAFF" "" . wrap "<box type=Top width=2 mt=2 color=#82AAFF>" "</box>" -- . clickable -- Hidden workspaces
              -- , ppHidden = xmobarColor "#82AAFF" "" . wrap "<box type=Top width=2 mt=2 color=#82AAFF>" "</box>" -- . clickable -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#82AAFF" "" -- . clickable
              -- , ppHiddenNoWindows = xmobarColor "#82AAFF" "" . wrap "*" ""  . clickable (zip myWorkspaces [1..]) -- . clickable     -- Hidden workspaces (no windows)
              , ppTitle = xmobarColor "#b3afc2" "" . shorten 60               -- Title of active window
              , ppSep =  "<fc=#666666> | </fc>"                    -- Separator character
              , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
              , ppExtras  = [windowCount]                                     -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                    -- order of things in xmobar
              }
        } `additionalKeysP` myKeys
