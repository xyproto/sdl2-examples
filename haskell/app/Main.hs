{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Foreign.C.Types
import SDL.Vect
import qualified SDL
import System.FilePath

import Paths_grumpycat (getDataFileName)
import System.Directory (getCurrentDirectory)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (620, 387)

main :: IO ()
main = do
  SDL.initializeAll
  win <- SDL.createWindow "Hello World!" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  ren <- SDL.createRenderer win (-1) SDL.defaultRenderer
  -- Load the image from ../img/grumpy-cat.bmp
  currentDirectory <- getCurrentDirectory
  let imageFileName = currentDirectory </> ".." </> "img" </> "grumpy-cat.bmp"
  putStrLn ("Loading " ++ imageFileName)
  bmp <- SDL.loadBMP imageFileName
  -- Create a texture from the surface
  tex <- SDL.createTextureFromSurface ren bmp
  -- Render the image in a loop for 2 seconds (20 * 100ms)
  mainLoop ren tex 20
  SDL.destroyWindow win
