{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Lib

main :: IO ()
main = do
  initializeAll
  win <- createWindow "Hello world!" defaultWindow
  ren <- createRenderer win (-1) defaultRenderer
  mainLoop ren
  destroyWindow win
