{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( mainLoop
    ) where

import SDL

mainLoop :: Renderer -> Texture -> Int -> IO ()
mainLoop ren tex n = do
  -- Present the texture and wait 100 ms
  SDL.clear ren
  SDL.copy ren tex Nothing Nothing
  SDL.present ren
  SDL.delay 100
  -- Call mainLoop recursively until n is 0
  if n > 0
    then mainLoop ren tex (n - 1)
    else return ()
