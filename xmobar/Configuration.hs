{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Configuration (config) where

import AConfig (AConfig (..), HstNm (..), hstNmCond)
import qualified Data.List
import Xmobar hiding (date)

dynNetwork cnf =
  Run $
    DynNetwork
      [ "-L",
        "0",
        "-H",
        "32000",
        "--normal",
        cl_fg cnf,
        "--high",
        cl_alert cnf,
        "-t",
        "<rxvbar> <txvbar>"
      ]
      50

multicpu cnf =
  Run $
    MultiCpu
      [ "-L",
        "0",
        "--minwidth",
        "2",
        "--low",
        cl_finecolor cnf,
        "--normal",
        cl_fg cnf,
        "--high",
        cl_alert cnf,
        "-t",
        "<total>%"
      ]
      50

memory cnf =
  Run $
    Memory
      [ "--normal",
        cl_fg cnf,
        "--high",
        cl_alert cnf,
        "--minwidth",
        "2",
        "-m",
        "2",
        "-L",
        "0",
        "-H",
        "90",
        "-t",
        "<usedratio>%"
      ]
      50

date = Run $ Date "%a %d %b %H:%M" "date" 600

alsa cnf =
  Run $
    Alsa
      "default"
      "Master"
      [ "--low",
        cl_fg cnf,
        "--normal",
        cl_fg cnf,
        "--high",
        cl_alert cnf,
        "-H",
        "100",
        "-t",
        "<status> <volume>%",
        "--minwidth",
        "2",
        "--",
        "--highs",
        "墳",
        "--mediums",
        "奔",
        "--lows",
        "奄",
        "--off",
        "婢",
        "--on",
        "",
        "--onc",
        cl_fg cnf,
        "--offc",
        cl_alert cnf
      ]

multicoretemp cnf =
  Run $
    MultiCoreTemp
      [ "-L",
        "25",
        "-H",
        "75",
        "--minwidth",
        "2",
        "--low",
        cl_finecolor cnf,
        "--normal",
        cl_fg cnf,
        "--high",
        cl_alert cnf,
        "-t",
        "<avg>°C"
      ]
      50

enzv =
  Run $
    WeatherX
      "ENZV"
      [ ("clear", "望"),
        ("sunny", "\xe30d"),
        ("mostly clear", "\xe37b"),
        ("mostly sunny", "\xe30c"),
        ("partly sunny", "\xe302"),
        ("fair", "\xe302"),
        ("cloudy", "\xe33d"),
        ("overcast", "\xe33d"),
        ("partly cloudy", "\xe379"),
        ("mostly cloudy", "\xe37e"),
        ("considerable cloudiness", "\xfa8f")
      ]
      ["-t", "<skyConditionS> <tempC>° <windMs>m/s <windCardinal> <windAzimuth> <pressure>"]
      600

xmonadLog = Run UnsafeXMonadLog

battery cnf =
  Run $
    BatteryP
      ["BAT0"]
      [ "-t",
        "<leftipat>",
        "-L",
        "10",
        "-H",
        "80",
        "-p",
        "3",
        "--minwidth",
        "3",
        "--",
        "--on-icon-pattern",
        "\xf58e<left>% <timeleft> <watts>",
        "--off-icon-pattern",
        "\xf58b<left>% <timeleft> <watts>",
        "--idle-icon-pattern",
        "\xf578",
        "-L",
        "-20",
        "-H",
        "-10",
        "-l",
        cl_fg cnf,
        "-m",
        cl_finecolor cnf,
        "-h",
        cl_alert cnf,
        "-p",
        cl_finecolor cnf,
        "-a",
        "notify-send -u critical 'Battery running out!!'",
        "-A",
        "3"
      ]
      50

trayerPadding = Run $ Com "/bin/sh" ["-c", "yes ' ' | tr -d '\\n' | head -c $((`xdotool search --onlyvisible --class 'Trayer' getwindowgeometry| sed -n 's/^\\s*Geometry: \\([0-9]\\+\\)x.*$/\\1/p'`/12))"] "trayerPadding" 50

nvidiaTemp = Run $ Com "nvidia-settings" ["-t", "-q", "[gpu:0]/GPUCoreTemp"] "nvidiaTemp" 50

btcPrice = Run $ Com "/bin/sh" ["-c", cryptoPrice "BTC-USD"] "btcPrice" 600

ethprice = Run $ Com "/bin/sh" ["-c", cryptoPrice "ETH-USD"] "ethprice" 600

hogwartsDiskUsg = Run $ DiskU [("/mnt/fastdisk", hddTmp "fastdisk"), ("/", hddTmp "root"), ("/boot", hddTmp "boot"), ("/mnt/bigdisk", hddTmp "bigdisk")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f", "▰", "-b", "▱", "-W", "6"] 100

coretemp = Run $ CoreTemp ["-t", "<core0>|<core1>C", "-L", "40", "-H", "60", "-l", "lightblue", "-n", "gray90", "-h", "red"] 50

nimbusDiskUsg = Run $ DiskU [("/", hddTmp "root"), ("/boot", hddTmp "boot")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f", "▰", "-b", "▱", "-W", "6"] 100

cryptoPrice :: [Char] -> [Char]
cryptoPrice pair = "curl 'https://api.coinbase.com/v2/prices/" ++ pair ++ "/spot?currency=USD' -s | jq '.data.amount' -r"

hddTmp :: [Char] -> [Char]
hddTmp hddName = hddName ++ " <free>/<size> <usedbar> "

alsaLol = "<action=`setSinkVolumeDefault.sh +1db` button=4><action=`setSinkVolumeDefault.sh -1db` button=5>%alsa:default:Master%</action></action> "

hanstopCmds cnf = [xmonadLog, alsa cnf, battery cnf, memory cnf, multicpu cnf, multicoretemp cnf, date]

hanstopTmpl :: [String]
hanstopTmpl =
  [ "%UnsafeXMonadLog%}{" ++ alsaLol,
    " %battery% ",
    " \xf85a %memory% ",
    " \xfb19 %multicpu% %multicoretemp% ",
    " %date%"
  ]

nimbusCmds cnf = [xmonadLog, btcPrice, ethprice, enzv, nimbusDiskUsg, alsa cnf, battery cnf, memory cnf, nvidiaTemp, multicpu cnf, coretemp, date, trayerPadding]

nimbusTpl :: [String]
nimbusTpl =
  [ "%UnsafeXMonadLog%}{" ++ alsaLol,
    " %ENZV% ",
    " %disku% ",
    " ETH %ethprice% ",
    " BTC %btcPrice% ",
    " \xf85a %memory% ",
    " \xf7e8 %nvidiaTemp%°C ",
    " \xfb19 %multicpu% %coretemp% ",
    " %battery% ",
    " %date% %trayerPadding%"
  ]

nimbusTplCompact :: [String]
nimbusTplCompact =
  ["%UnsafeXMonadLog%}{" ++ alsaLol, " %battery% ", " %date% %trayerPadding%"]

stationaryCmds cnf = [xmonadLog, hogwartsDiskUsg, ethprice, btcPrice, enzv, alsa cnf, nvidiaTemp, memory cnf, multicpu cnf, multicoretemp cnf, date]

hogwartsTpl :: [Char]
hogwartsTpl =
  "%UnsafeXMonadLog%}\
  \{%disku% ETH %ethprice% | BTC %btcPrice% | %ENZV% | "
    ++ alsaLol
    ++ " | \xf7e8 %nvidiaTemp%°C | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | <action=`~/bin/runner.sh` button=1>%date%</action>"

config :: AConfig -> Config
config cnf =
  let nizSep = Data.List.intercalate ("<fc=" ++ cl_accent cnf ++ ">|</fc>")
   in Xmobar.defaultConfig
        { verbose = False,
          textOutput = False,
          wmClass = "xmobar",
          wmName = "xmobar",
          border = NoBorder,
          -- borderColor = cl_accent cnf,
          -- borderWidth = 1,
          textOffsets = [],
          -- font = cl_font cnf,
          font = cl_font_pango cnf,
          -- additionalFonts = [],
          bgColor = cl_bg cnf,
          fgColor = cl_fg cnf,
          alpha = 160,
          signal = SignalChan Nothing,
          dpi = fromIntegral (cl_dpi cnf),
          -- , position = Top
          position = TopSize L 100 (cl_barHeight cnf),
          textOffset = -1,
          iconOffset = -1,
          lowerOnStart = True,
          pickBroadest = False,
          persistent = False,
          hideOnStart = False,
          iconRoot = "",
          allDesktops = True,
          overrideRedirect = True,
          commands =
            hstNmCond
              cnf
              HstNm
                { hst_hogwarts = stationaryCmds cnf,
                  hst_hanstop = hanstopCmds cnf,
                  hst_nimbus2k = nimbusCmds cnf,
                  hst_other = nimbusCmds cnf
                },
          sepChar = "%",
          alignSep = "}{",
          template =
            hstNmCond
              cnf
              HstNm
                { hst_hogwarts = hogwartsTpl,
                  hst_hanstop = nizSep hanstopTmpl,
                  hst_nimbus2k = nizSep (if cl_compact cnf then nimbusTplCompact else nimbusTpl),
                  hst_other = nizSep (if cl_compact cnf then nimbusTplCompact else nimbusTpl)
                }
        }
