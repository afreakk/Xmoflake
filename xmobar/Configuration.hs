{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Configuration (config) where
import Xmobar hiding (date)
import AConfig (AConfig (..), HstNm(..), hstNmCond)

dynnetwork cnf = Run $ DynNetwork
    ["-L", "0",
     "-H", "32000",
     "--normal", cl_fg0 cnf,
     "--high",cl_red cnf,
     "-t", "<rxvbar> <txvbar>"
    ] 50

multicpu cnf = Run $ MultiCpu
    ["-L", "0",
     "--minwidth", "2",
     "--low", cl_aqua cnf,
     "--normal", cl_fg0 cnf,
     "--high", cl_red cnf,
     "-t", "<total>%"
    ] 50

memory cnf = Run $ Memory
    ["--normal", cl_fg0 cnf,
     "--high", cl_red cnf,
     "--minwidth", "2",
     "-m", "2",
     "-L", "0",
     "-H", "90",
     "-t","<usedratio>%"
    ] 50

date = Run $ Date "%a %d %b %H:%M" "date" 600

alsa cnf = Run $ Alsa "default" "Master"
    ["--low", cl_fg0 cnf,
     "--normal", cl_fg0 cnf,
     "--high", cl_red cnf,
     "-H", "100",
     "-t", "<status> <volume>%",
     "--minwidth", "2",
     "--",
     "--highs", "墳",
     "--mediums", "奔",
     "--lows", "奄",
     "--off", "婢",
     "--on", "",
     "--onc", cl_fg0 cnf,
     "--offc", cl_red cnf
    ]
multicoretemp cnf = Run $ MultiCoreTemp
    ["-L", "25",
     "-H", "75",
     "--minwidth", "2",
     "--low", cl_aqua cnf,
     "--normal", cl_fg0 cnf,
     "--high", cl_red cnf,
     "-t", "<avg>°C"
    ] 50

enzv = Run $ WeatherX "ENZV"
    [ ("clear", "望")
    , ("sunny", "\xe30d")
    , ("mostly clear", "\xe37b")
    , ("mostly sunny", "\xe30c")
    , ("partly sunny", "\xe302")
    , ("fair", "\xe302")
    , ("cloudy","\xe33d")
    , ("overcast","\xe33d")
    , ("partly cloudy", "\xe379")
    , ("mostly cloudy", "\xe37e")
    , ("considerable cloudiness", "\xfa8f")]
  ["-t", "<skyConditionS> <tempC>° <windMs>m/s <windCardinal> <windAzimuth> <pressure>"] 600
xmonadlog  = Run UnsafeXMonadLog
battery cnf = Run $ BatteryP ["BAT0"]
    ["-t", "<leftipat>",
     "-L", "10", "-H", "80", "-p", "3",
     "--minwidth", "3",
     "--",
     "--on-icon-pattern", "\xf58e<left>% <timeleft> <watts>",
     "--off-icon-pattern", "\xf58b<left>% <timeleft> <watts>",
     "--idle-icon-pattern", "\xf578",
     "-L", "-20", "-H", "-10",
     "-l", cl_fg0 cnf, "-m", cl_aqua cnf, "-h", cl_red cnf, "-p", cl_green cnf,
     "-a", "notify-send -u critical 'Battery running out!!'",
     "-A", "3"]
    50

trayerPadding = Run $ Com "/bin/sh" ["-c", "yes ' ' | tr -d '\\n' | head -c $((`xdotool search --onlyvisible --class 'Trayer' getwindowgeometry| sed -n 's/^\\s*Geometry: \\([0-9]\\+\\)x.*$/\\1/p'`/12))"] "trayerPadding" 50
nvidiaTemp = Run $ Com "nvidia-settings" ["-t","-q","[gpu:0]/GPUCoreTemp" ] "nvidiaTemp" 50
btcprice = Run $ Com "/bin/sh" ["-c", cryptoPrice "BTC-USD"] "btcprice" 600
ethprice = Run $ Com "/bin/sh" ["-c", cryptoPrice "ETH-USD"] "ethprice" 600
hogwartsDisku = Run $ DiskU [("/mnt/fastdisk", hddTmp "fastdisk"), ("/", hddTmp "root"), ("/boot", hddTmp "boot"), ("/mnt/bigdisk", hddTmp "bigdisk")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f","▰", "-b","▱", "-W","6"] 100
coretemp = Run $ CoreTemp ["-t", "<core0>|<core1>C", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "gray90", "-h", "red"] 50
nimbusDisku = Run $ DiskU [("/", hddTmp "root"), ("/boot", hddTmp "boot")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f","▰", "-b","▱", "-W","6"] 100

cryptoPrice :: [Char] -> [Char]
cryptoPrice pair = "curl 'https://api.coinbase.com/v2/prices/"++pair++"/spot?currency=USD' -s | jq '.data.amount' -r"

hddTmp :: [Char] -> [Char]
hddTmp hddName = hddName ++" <free>/<size> <usedbar> |"

alsaLol = "<action=`setSinkVolumeDefault.sh +1db` button=4><action=`setSinkVolumeDefault.sh -1db` button=5>%alsa:default:Master%</action></action>"

hanstopCmds cnf = [xmonadlog, alsa cnf, battery cnf, memory cnf, multicpu cnf, multicoretemp cnf, date]
hanstopTmpl :: [Char]
hanstopTmpl =
  "%UnsafeXMonadLog%}\
  \{" ++ alsaLol ++ " | %battery% | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | %date%"

nimbusCmds cnf = [xmonadlog, btcprice, ethprice, enzv, nimbusDisku ,alsa cnf,  battery cnf, memory cnf, nvidiaTemp, multicpu cnf, coretemp, date, trayerPadding]
nimbusTpl :: [Char]
nimbusTpl =
  "%UnsafeXMonadLog%}\
  \{" ++ alsaLol ++ " | %ENZV% | %disku% ETH %ethprice% | BTC %btcprice% | \xf85a %memory% | \xf7e8 %nvidiaTemp%°C | \xfb19 %multicpu% %coretemp% | %battery% | %date% %trayerPadding%"

stationaryCmds cnf = [xmonadlog, hogwartsDisku, ethprice, btcprice, enzv, alsa cnf, nvidiaTemp, memory cnf, multicpu cnf, multicoretemp cnf, date]
stationaryTmpl :: [Char]
stationaryTmpl = 
  "%UnsafeXMonadLog%}\
  \{%disku% ETH %ethprice% | BTC %btcprice% | %ENZV% | " ++ alsaLol ++ " | \xf7e8 %nvidiaTemp%°C | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | <action=`~/bin/runner.sh` button=1>%date%</action>"

config :: AConfig -> Config
config cnf =
  Config { verbose = False
         , textOutput = False
         , wmClass = "xmobar"
         , wmName = "xmobar"
         , border = NoBorder
         , borderColor = cl_orange cnf
         , borderWidth = 1
         , textOffsets = []
         , font = cl_font cnf
         , additionalFonts = []
         , bgColor = cl_bg cnf
         , fgColor = cl_fg0 cnf
         , alpha = 200
         , signal = (SignalChan Nothing)
         -- , position = Top
         , position = TopSize L 100 (cl_barHeight cnf)
         , textOffset = -1
         , iconOffset = -1
         , lowerOnStart = True
         , pickBroadest = False
         , persistent = False
         , hideOnStart = False
         , iconRoot = ""
         , allDesktops = True
         , overrideRedirect = True
         , commands = hstNmCond cnf HstNm
           { hst_hogwarts = stationaryCmds cnf
           , hst_hanstop = hanstopCmds cnf
           , hst_nimbus2k = nimbusCmds cnf
           , hst_other  = nimbusCmds cnf
           }
         , sepChar = "%"
         , alignSep = "}{"
         , template = hstNmCond cnf HstNm
           { hst_hogwarts = stationaryTmpl
           , hst_hanstop = hanstopTmpl
           , hst_nimbus2k = nimbusTpl
           , hst_other  = nimbusTpl
           }
         }
