-- Glyphs: https://fontawesome.com/v4/cheatsheet/
-- Plugins: https://codeberg.org/xmobar/xmobar/src/branch/master/doc/plugins.org

Config { overrideRedirect = True
       -- , font     = "xft:iosevka-13"
       -- , font = "xft:Ubuntu:weight=bold:pixelsize=15:antialias=true:hinting=true"
       , font = "xft:Inconsolata:italic:size=11"
       -- , font = "xft:terminus:size=11"
      , additionalFonts = [ "xft:FontAwesome:size=11" ]
      , bgColor = "#282c34"
      , fgColor = "#bbc2cf"
      , position = TopW L 100
      , alpha    = 200
      , iconRoot = "/home/lokesh/.config/xmonad/xpm"
      , commands = [ Run Cpu
                       [ "-t", "<fn=1>\xf26c</fn> <total>%"
                       , "-L", "5" , "-H", "50"
                       , "-h", "red" , "-n", "yellow", "-l", "green"
                       ] 10
                   , Run Weather "VOBG" ["-t", "<fc=#000000><fn=1>\xf2c8</fn></fc> <tempC>°C"] 3600
                   , Run MultiCoreTemp
                       [ "-t", "<max>°C"
                       , "-L", "60", "-H", "80"
                       , "-h", "red", "-n", "yellow", "-l", "green"
                       ] 50
                   , Run Memory
                       ["-t", "<fn=1>\xf1c0</fn> <usedratio>%"
                       , "-L", "40", "-H", "80"
                       , "-h", "red", "-n", "yellow", "-l", "green"
                       ] 10
                   , Run DynNetwork
                       [ "-t", "<fc=#4db5bd><fn=1></fn></fc> <rx>, <fc=#c678dd><fn=1></fn></fc> <tx>"
                       , "-H","200" , "-L","10"
                       , "-h","#bbc2cf" , "-l","#bbc2cf" , "-n","#bbc2cf"
                       , "-S", "True"
                       ] 50
                  , Run Battery
                      [ "-t" , "<acstatus>"
                      , "-L", "20" , "-H", "80"
                      , "--"
                      , "-L", "30" , "-H", "70", "-o", ""
                      , "-O", "<fc=#00cc00><fn=1>\xf0e7</fn> <left>% (<timeleft>)</fc>"   -- charging status
                      , "-i", "<fc=#00cc00><fn=1>\xf240</fn> Charged</fc>"              -- charged status
                      , "--lows", "<fc=#cc0000><fn=1>\xf243</fn> <left>% (<timeleft>)</fc>"
                      , "--mediums", "<fc=#cccc00><fn=1>\xf242</fn> <left>% (<timeleft>)</fc>"
                      , "--highs", "<fc=#00cc00><fn=1>\xf241</fn> <left>% (<timeleft>)</fc>"
                      , "-A", "10" , "-a", "dunstify -u critical 'Battery running out!!'"
                      ] 50
                   , Run Date "<action=`gnome-calendar` button=1><fn=1>\xf073</fn> %a %d-%m-%Y <fc=#8be9fd>%H:%M:%S</fc></action>" "date" 10
                   , Run Com "/home/lokesh/.config/xmonad/scripts/trayer-padding-icon.sh" [] "trayerpad" 20
                   , Run Locks
                   , Run UnsafeXMonadLog
                   ]
      , sepChar  = "%"
      , alignSep = "}{"
      , template = " <icon=/haskell_20.xpm/> %UnsafeXMonadLog% }{ %locks%  <action=`alacritty -e /home/lokesh/.config/xmonad/scripts/weather.sh` button=1>%VOBG%</action>  <action=`alacritty -e htop` button=1>%cpu%, %multicoretemp%  %memory%</action>  %dynnetwork%  %battery%  %date% | %trayerpad% "
      }
