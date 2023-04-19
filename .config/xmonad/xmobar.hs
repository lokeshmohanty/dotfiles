Config { overrideRedirect = True
       -- , font     = "xft:iosevka-13"
       -- , font = "xft:Ubuntu:weight=bold:pixelsize=15:antialias=true:hinting=true"
       , font = "xft:Inconsolata:italic:size=11"
       -- , font = "xft:terminus:size=11"
      , additionalFonts = [ "xft:FontAwesome:size=11", "xft:NerdFont:size=11" ]
      , bgColor = "#282c34"
      , fgColor = "#bbc2cf"
      , position = TopW L 100
      , alpha    = 150
      , iconRoot = "/home/lokesh/.config/xmonad/xpm"
      , commands = [ Run Weather "EGPF"
                       [ "--template", "<weather> <tempC>°C"
                       , "-L", "0"
                       , "-H", "25"
                       , "--low"   , "lightblue"
                       , "--normal", "#f8f8f2"
                       , "--high"  , "red"
                       ] 36000
                   , Run Cpu
                       [ "-L", "3"
                       , "-H", "50"
                       , "--high"  , "red"
                       , "--normal", "green"
                       ] 10
                   , Run MultiCoreTemp
                       [ "-t", "Temp: <avg>°C - <avgpc>%"
                       , "-L", "60", "-H", "80"
                       , "-l", "green", "-n", "yellow", "-h", "red"
                       , "--", "--mintemp", "20", "--maxtemp", "100"
                       ] 50
                   , Run Memory ["--template", "Mem: <usedratio>%"] 10
                   , Run Swap [] 10
                   , Run DynNetwork
                       [ "-t", "<fc=#4db5bd><fn=1></fn></fc> <rx>, <fc=#c678dd><fn=1></fn></fc> <tx>"
                       , "-H","200"
                       , "-L","10"
                       , "-h","#bbc2cf"
                       , "-l","#bbc2cf"
                       , "-n","#bbc2cf"
                       , "-S", "True"
                       ] 50
                  , Run Battery
                      [ "-t" , "<acstatus>"
                      , "-L", "30", "-l", "#fb4934" -- #ff5555
                      , "-H", "80", "-h", "#98be65"
                      , "-n", "#bbc2cf"
                      , "--"
                      , "-o", "<fc=#cccc00><fn=1></fn> <left>% (<timeleft>)</fc>" -- discharging status
                      , "-O", "<fc=#cccc00><fn=1></fn> <left>% (Charging)</fc>"   -- charging status
                      -- , "-O", "<fc=#cccc00><fn=1>\uf12a5</fn> <left>% (Charging)</fc>"   -- charging status
                      , "-i", "<fc=#cccc00><fn=1></fn> Charged</fc>"              -- charged status
                      , "-a", "notify-send -u critical 'Battery running out!!'"
                      , "-A", "30"
                      ] 50
                   , Run Date "%a %d-%m-%Y <fc=#8be9fd>%H:%M:%S</fc>" "date" 10
                   , Run Com "/home/lokesh/.config/xmonad/scripts/trayer-padding-icon.sh" [] "trayerpad" 20
                   , Run Locks
                   , Run UnsafeXMonadLog
                   ]
      , sepChar  = "%"
      , alignSep = "}{"
      , template = " <icon=haskell_20.xpm/> %UnsafeXMonadLog% }{ %locks% | %cpu% | %multicoretemp% | %memory% | %dynnetwork% | %battery% | %date% | %trayerpad% "
      }
