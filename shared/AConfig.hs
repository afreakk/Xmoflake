{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AConfig (getConfig, AConfig (..), HstNm (..), hstNmCond) where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.Key as DAK
import qualified Data.Char
import Data.List
import Data.Maybe
import GHC.Generics
import Network.HostName hiding (HostName)
-- import SimpleCmd
import System.Directory (getHomeDirectory)
import System.FilePath (combine)

data HostName = Hanstop | Nimbus2k | Hogwarts | Other deriving (Show, Generic)

hstNmCond :: AConfig -> HstNm a -> a
hstNmCond AConfig {cl_hostName = Hanstop} x = hst_hanstop x
hstNmCond AConfig {cl_hostName = Hogwarts} x = hst_hogwarts x
hstNmCond AConfig {cl_hostName = Nimbus2k} x = hst_nimbus2k x
hstNmCond _ x = hst_other x

data HstNm a = HstNm
  { hst_hogwarts :: a,
    hst_hanstop :: a,
    hst_nimbus2k :: a,
    hst_other :: a
  }

stringToHostName "hanstop" = Hanstop
stringToHostName "hogwarts" = Hogwarts
stringToHostName "nimbus2k" = Nimbus2k
stringToHostName _ = Other

data AConfig = AConfig
  { cl_bg :: String,
    cl_alert :: String,
    cl_finecolor :: String,
    cl_accent :: String,
    cl_fg :: String,
    cl_font_pango :: String,
    cl_font :: String,
    cl_font_big :: String,
    cl_font_very_big :: String,
    cl_barHeight :: Int,
    cl_tabHeight :: Int,
    cl_xpHeight :: Int,
    cl_hostName :: HostName,
    cl_gsCellWidth :: Integer,
    cl_gsCellWidthBig :: Integer,
    cl_gsCellHeightBig :: Integer,
    cl_gsCellHeight :: Integer,
    cl_dpi :: Integer,
    cl_windowTitleLength :: Int,
    cl_compact :: Bool
  }
  deriving (Show, Generic)

instance FromJSON HostName where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = Data.List.map Data.Char.toLower
        }

instance ToJSON HostName

instance FromJSON AConfig

instance ToJSON AConfig

headWithDefault [] = "Xft.dpi:        200"
headWithDefault x = Data.List.head x

sndOfThing [fst, snd] = snd
sndOfThing x = "200"

pangoFont size = "Hack Nerd Font " ++ size

sizedFont size = "xft:Hack Nerd Font:size=" ++ size ++ ":Regular:antialias=true"

getConfig :: IO AConfig
getConfig = do
  hostName <- fmap stringToHostName getHostName
  -- xrdbQStr <- cmd "xrdb" ["-query"]
  -- let xrdbQStr = fromStdout xrdbOut
  -- let lined = lines  xrdbQStr
  -- let dpiLine = headWithDefault $ filter (isInfixOf "Xft.dpi:") lined
  -- let dpiStr = sndOfThing (words dpiLine)
  -- let dpi = readMaybe dpiStr :: Maybe Integer
  -- let dpi = Just 100
  homeDir <- getHomeDirectory
  jsonConfig <- decodeFileStrict (combine homeDir ".config/xmoflake.json") :: IO (Maybe AConfig)
  -- fallback to hardcoded config if json can't decode / doesn't exist.
  let cfg = fromMaybe (_getConfig hostName) jsonConfig
  -- encodeFile "/home/afreak/xmonad.json" cfg
  return cfg
  where
    _getConfig hostName =
      AConfig
        { cl_bg = ifIsLightTheme "#fbf1c7" "#282828",
          cl_alert = "#cc241d",
          cl_finecolor = "#689d6a",
          cl_accent = "#b16286",
          cl_fg = ifIsLightTheme "#282828" "#fbf1c7",
          cl_font_pango = pangoFont "14",
          cl_font = sizedFont "10",
          cl_font_big = sizedFont "30",
          cl_font_very_big = sizedFont "70",
          cl_barHeight = 30,
          cl_xpHeight = 26,
          cl_tabHeight = 26,
          cl_hostName = hostName,
          cl_gsCellWidth = 400,
          cl_gsCellWidthBig = 500,
          cl_gsCellHeight = 50,
          cl_gsCellHeightBig = 80,
          cl_dpi = 100,
          cl_windowTitleLength = 75,
          cl_compact = False
        }
      where
        ifIsLightTheme lightValue darkValue = case hostName of
          -- "hanstop" -> lightValue
          _ -> darkValue
