module Main where

import Xmobar
import Configuration (config)
import AConfig (getConfig)

main :: IO ()
main = do
  cnf <- getConfig
  xmobar $ config cnf
