{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( mainLoop
    ) where

import SDL
import Linear (V4(..))
import Control.Monad (unless)

mainLoop :: Renderer -> IO ()
mainLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (mainLoop renderer)
