{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Configuration (config) where

import AConfig (AConfig (..), HstNm (..), hstNmCond)
import qualified Data.List
import Xmobar hiding (date)

-- Helper for common color threshold arguments
colorArgs :: AConfig -> [String]
colorArgs cnf = ["--low", cl_finecolor cnf, "--normal", cl_fg cnf, "--high", cl_alert cnf]

-- Helper for normal/high only (no low threshold)
colorArgsNoLow :: AConfig -> [String]
colorArgsNoLow cnf = ["--normal", cl_fg cnf, "--high", cl_alert cnf]

-- Colored icon helper for clean aesthetic
ic :: String -> String -> String
ic color i = "<fc=" ++ color ++ ">" ++ i ++ "</fc>"

-- Section with colored icon
section :: String -> String -> String -> String
section color i content = " " ++ ic color i ++ " " ++ content ++ " "

-- Powerline separator
sep :: AConfig -> String
sep cnf = "<fc=" ++ cl_accent cnf ++ ">  </fc>"

dynNetwork cnf =
  Run $
    DynNetwork
      ( ["-L", "0", "-H", "32000", "-t", "<rxvbar> <txvbar>"]
          ++ colorArgsNoLow cnf
      )
      50

multicpu cnf =
  Run $
    MultiCpu
      ( ["-L", "0", "--minwidth", "2", "-t", "<total>%"]
          ++ colorArgs cnf
      )
      50

memory cnf =
  Run $
    Memory
      ( ["--minwidth", "2", "-m", "2", "-L", "0", "-H", "90", "-t", "<usedratio>%"]
          ++ colorArgsNoLow cnf
      )
      50

date = Run $ Date "%a %d %b %H:%M" "date" 600

-- Event-based volume monitor via PipeReader (fed by systemd service)
vol = Run $ PipeReader "/run/user/1000/xmobar-volume" "vol"

multicoretemp cnf =
  Run $
    MultiCoreTemp
      ( ["-L", "25", "-H", "75", "--minwidth", "2", "-t", "<max>°C"]
          ++ colorArgs cnf
      )
      50

enzv =
  Run $
    WeatherX
      "ENZV"
      [ ("clear", "🌣"),
        ("sunny", "🌣"),
        ("mostly clear", "🌤"),
        ("mostly sunny", "🌤"),
        ("partly sunny", "⛅"),
        ("fair", "🌑"),
        ("cloudy", "☁"),
        ("overcast", "☁"),
        ("partly cloudy", "⛅"),
        ("mostly cloudy", "🌧"),
        ("considerable cloudiness", "⛈"),
        ("obscured", "🌫")
      ]
      -- ["-t", "<skyConditionS> <tempC>° <windKnots>kn <windCardinal> <rh>%"]
      ["-t", "<tempC>° <windKnots>kn <windCardinal> <rh>%"]
      -- look out the window, dont need skyConditionS etc
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
        "<fc=" ++ cl_finecolor cnf ++ ">\xf0084</fc> <left>% <timeleft> <watts>",
        "--off-icon-pattern",
        "<fc=" ++ cl_accent cnf ++ ">\xf008c</fc> <left>% <timeleft> <watts>",
        "--idle-icon-pattern",
        "<fc=" ++ cl_finecolor cnf ++ ">\xf0079</fc>",
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

trayerPadding = Run $ NamedXPropertyLog "_XMONAD_TRAYPAD" "trayerPadding"

nvidiaTemp = Run $ Com "nvidia-settings" ["-t", "-q", "[gpu:0]/GPUCoreTemp"] "nvidiaTemp" 50

btcPrice = Run $ Com "/bin/sh" ["-c", cryptoPrice "BTC-USD"] "btcPrice" 600

ethprice = Run $ Com "/bin/sh" ["-c", cryptoPrice "ETH-USD"] "ethprice" 600

hogwartsDiskUsg = Run $ DiskU [("/mnt/fastdisk", hddTmp "fastdisk"), ("/", hddTmp "root"), ("/boot", hddTmp "boot"), ("/mnt/bigdisk", hddTmp "bigdisk")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f", "▰", "-b", "▱", "-W", "6"] 100

nimbusDiskUsg cnf = Run $ DiskU [("/", hddTmp (ic (cl_accent cnf) "\xf10b5")), ("/boot", hddTmp (ic (cl_accent cnf) "\xf10ea"))] ["-L", "20", "-H", "50", "-m", "1", "-p", "3", "-f", "▰", "-b", "▱", "-W", "6"] 100

cryptoPrice :: [Char] -> [Char]
cryptoPrice pair = "curl 'https://api.coinbase.com/v2/prices/" ++ pair ++ "/spot?currency=USD' -s | jq '.data.amount' -r | cut -d . -f 1"

hddTmp :: [Char] -> [Char]
hddTmp hddName = hddName ++ " <free>/<size> <usedbar> "

mpris cnf = Run $ Mpris2 "playerctld" ["-T", "38", "-E", "…", "-M", "25", "-e", ">", "-t", ic (cl_accent cnf) "\xf001" ++ " <artist>/<title>", "-x", ""] 10

volClickable = "<action=`i3-volume -y -p -n -P -C -s @DEFAULT_SINK@ up 1` button=4><action=`i3-volume -y -p -n -P -C -s @DEFAULT_SINK@ down 1` button=5>%vol%</action></action> "

batteryClickable = popupAction "battery-popup" " %battery% "

-- Popup toggle action helper for QuickShell system monitoring popups
popupAction :: String -> String -> String
popupAction target content = "<action=`qs ipc -c system-popups call " ++ target ++ " toggle` button=1>" ++ content ++ "</action>"

hanstopCmds cnf = [xmonadLog, vol, battery cnf, memory cnf, multicpu cnf, multicoretemp cnf, date, trayerPadding]

hanstopTmpl :: AConfig -> [String]
hanstopTmpl cnf =
  [ "%UnsafeXMonadLog%}{" ++ volClickable,
    batteryClickable,
    popupAction "memory-popup" (section (cl_accent cnf) "\xf035b" "%memory%"),
    popupAction "cpu-popup" (section (cl_accent cnf) "\xe266" "%multicpu%" ++ ic (cl_finecolor cnf) " \xf2c9 " ++ "%multicoretemp%"),
    popupAction "calendar-popup" (section (cl_finecolor cnf) "\xf073" "%date%") ++ "%trayerPadding%"
  ]

nimbusCmds cnf = [xmonadLog, btcPrice, ethprice, enzv, nimbusDiskUsg cnf, vol, battery cnf, memory cnf, nvidiaTemp, multicpu cnf, multicoretemp cnf, date, trayerPadding, mpris cnf]

nimbusTpl :: AConfig -> [String]
nimbusTpl cnf =
  [ "%UnsafeXMonadLog%}{" ++ volClickable,
    " %mpris2%",
    section (cl_accent cnf) "\xf0599" "%ENZV%",
    " %disku%",
    section (cl_finecolor cnf) "\xf086a" "%ethprice%",
    section (cl_finecolor cnf) "\xf0813" "%btcPrice%",
    popupAction "memory-popup" (section (cl_accent cnf) "\xf035b" "%memory%"),
    popupAction "gpu-popup" (section (cl_accent cnf) "\xf108" "%nvidiaTemp%°C"),
    popupAction "cpu-popup" (section (cl_accent cnf) "\xe266" "%multicpu%" ++ ic (cl_finecolor cnf) " \xf2c9 " ++ "%multicoretemp%"),
    batteryClickable,
    popupAction "calendar-popup" (section (cl_finecolor cnf) "\xf073" "%date%") ++ "%trayerPadding%"
  ]

nimbusTplCompact :: AConfig -> [String]
nimbusTplCompact cnf =
  [ "%UnsafeXMonadLog%}{" ++ volClickable,
    batteryClickable,
    popupAction "calendar-popup" (section (cl_finecolor cnf) "\xf073" "%date%") ++ "%trayerPadding%"
  ]

stationaryCmds cnf = [xmonadLog, hogwartsDiskUsg, ethprice, btcPrice, enzv, vol, nvidiaTemp, memory cnf, multicpu cnf, multicoretemp cnf, date]

hogwartsTpl :: AConfig -> String
hogwartsTpl cnf =
  "%UnsafeXMonadLog%}{"
    ++ section (cl_accent cnf) "\xf0a0" "%disku%"
    ++ sep cnf
    ++ section (cl_finecolor cnf) "\xf086a" "%ethprice%"
    ++ sep cnf
    ++ section (cl_finecolor cnf) "\xf0813" "%btcPrice%"
    ++ sep cnf
    ++ section (cl_accent cnf) "\xf0599" "%ENZV%"
    ++ sep cnf
    ++ volClickable
    ++ sep cnf
    ++ popupAction "gpu-popup" (section (cl_accent cnf) "\xf108" "%nvidiaTemp%°C")
    ++ sep cnf
    ++ popupAction "memory-popup" (section (cl_accent cnf) "\xf035b" "%memory%")
    ++ sep cnf
    ++ popupAction "cpu-popup" (section (cl_accent cnf) "\xe266" "%multicpu%" ++ ic (cl_finecolor cnf) " \xf2c9 " ++ "%multicoretemp%")
    ++ sep cnf
    ++ popupAction "calendar-popup" (" " ++ ic (cl_finecolor cnf) "\xf073" ++ " %date% ")

config :: AConfig -> Config
config cnf =
  let pwrSep = Data.List.intercalate (sep cnf)
   in Xmobar.defaultConfig
        { verbose = False,
          textOutput = False,
          wmClass = "xmobar",
          wmName = "xmobar",
          border = NoBorder,
          textOffsets = [],
          font = cl_font_pango cnf,
          bgColor = cl_bg cnf,
          fgColor = cl_fg cnf,
          alpha = 255,
          signal = SignalChan Nothing,
          dpi = fromIntegral (cl_dpi cnf),
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
                { hst_hogwarts = hogwartsTpl cnf,
                  hst_hanstop = pwrSep (hanstopTmpl cnf),
                  hst_nimbus2k = pwrSep (if cl_compact cnf then nimbusTplCompact cnf else nimbusTpl cnf),
                  hst_other = pwrSep (if cl_compact cnf then nimbusTplCompact cnf else nimbusTpl cnf)
                }
        }
