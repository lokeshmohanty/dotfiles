-- Glyphs: https://fontawesome.com/v4/cheatsheet/
-- Plugins: https://codeberg.org/xmobar/xmobar/src/branch/master/doc/plugins.org

-- Required programs for custom actions
-- Weather -> curl, alacritty (terminal)
-- Cpu, memory -> htop, alacritty (terminal)
-- Battery -> tlpui (from flatpak)
-- Date -> gnome-calendar

Config { overrideRedirect = True
       -- , font     = "xft:iosevka-13"
       -- , font = "xft:Ubuntu:weight=bold:pixelsize=15:antialias=true:hinting=true"
       , font = "xft:Inconsolata:italic:size=11"
       -- , font = "xft:terminus:size=11"
      , additionalFonts = [ "xft:FontAwesome:size=11", "xft:Mononoki Nerd Font:pixelsize=11:antialias=true:hinting=true" ]
      , bgColor = "#282c34"
      , fgColor = "#bbc2cf"
      , position = TopW L 100
      , alpha    = 200
      -- , iconRoot = "/home/lokesh/.config/xmonad/xpm"
      , commands = [ Run Cpu
                       [ "-t", "<fn=1>\xf26c</fn> <total>%"
                       , "-L", "5" , "-H", "50"
                       , "-h", "red", "-n", "yellow", "-l", "#ecbe7b"
                       ] 10
                   , Run MultiCoreTemp
                       [ "-t", "<max>°C"
                       , "-L", "60", "-H", "80"
                       , "-h", "red", "-n", "yellow", "-l", "#ecbe7b"
                       ] 50
                   , Run Memory
                       ["-t", "<fn=1>\xf1c0</fn> <usedratio>%"
                       , "-L", "40", "-H", "80"
                       , "-h", "red", "-n", "#c678dd", "-l", "#c68c53"
                       ] 10
                   , Run Weather "VOBG" ["-t", "<fn=1>\xf2c8</fn> <tempC>°C"] 3600
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
                      , "-O", "<fc=#cccc00><fn=1>\xf0e7</fn> <left>% (<timeleft>)</fc>"   -- charging status
                      , "-i", "<fc=#cccc00><fn=1>\xf240</fn> Charged</fc>"              -- charged status
                      , "--lows", "<fc=#cc0000><fn=1>\xf243</fn> <left>% (<timeleft>)</fc>"
                      , "--mediums", "<fc=#cccc00><fn=1>\xf242</fn> <left>% (<timeleft>)</fc>"
                      , "--highs", "<fc=#cccc00><fn=1>\xf241</fn> <left>% (<timeleft>)</fc>"
                      , "-A", "10" , "-a", "dunstify -u critical 'Battery running out!!'"
                      ] 50
                   , Run Date "<fn=1>\xf073</fn> %a %d %b %H:%M" "date" 10
                   , Run Com "/home/lokesh/.config/xmonad/scripts/trayer-padding-icon.sh" [] "trayerpad" 20
                   , Run DiskU [("/", "<fn=1>\xf019</fn> <free> free")] [] 600
                   , Run Uptime ["-t", "<fn=1>\xf0aa</fn> <hours>h <minutes>m"] 600
                   , Run Locks
                   , Run UnsafeXMonadLog
                   ]
      , sepChar  = "%"
      , alignSep = "}{"
      , template = "<icon=/home/lokesh/.dotfiles/.config/xmonad/xpm/haskell_20.xpm/> %UnsafeXMonadLog% }{ \
            \ %locks% \
            \<action=`alacritty -e /home/lokesh/.config/xmonad/scripts/weather.sh` button=1>\
            \ <box type=Bottom width=2 mb=2 color=#ffffb3><fc=#ffffb3>%VOBG%</fc></box> \
            \</action>\
            \<action=`alacritty -e htop` button=1>\
            \ <box type=Bottom width=2 mb=2 color=#ecbe7b><fc=#ecbe7b>%cpu%, %multicoretemp%</fc></box> \
            \ <box type=Bottom width=2 mb=2 color=#c68c53><fc=#c68c53>%memory%</fc></box> \
            \</action>\
            \ <box type=Bottom width=2 mb=2 color=#98be65><fc=#98be65>%uptime%</fc></box> \
            \<action=`flatpak run com.github.d4nj1.tlpui` button=1>\
            \ <box type=Bottom width=2 mb=2 color=#cccc00><fc=#cccc00>%battery%</fc></box> \
            \</action>\
            \<action=`gnome-calendar` button=1>\
            \ <box type=Bottom width=2 mb=2 color=#46d9ff><fc=#46d9ff>%date%</fc></box> \
            \</action>\
            \ %trayerpad%"
            -- other colors: #c678dd (pink), #00cc00 (green), #a9a1e1 (green)
      }
