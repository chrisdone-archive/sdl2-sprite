{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text as T
import           Options.Applicative.Simple
import           SDL
import qualified SDL.Sprite
import           Text.Read

main :: IO ()
main = do
  ((fp, dim, ms), ()) <-
    simpleOptions
      "0.0.0"
      "sdl2-sprite"
      "Animate sprites with sdl2"
      ((,,) <$> argument str (metavar "FILE") <*>
       argument
         (eitherReader
            (\i ->
               case splitOn "x" i of
                 [w, h] -> V2 <$> readEither w <*> readEither h
                 _ -> Left "invalid WIDTHxHEIGHT specification"))
         (metavar "WIDTHxHEIGHT") <*>
       (fmap
          (\fps -> 1000 / fps)
          (option
             auto
             (long "fps" <> value (15 :: Double) <> help "Frames per second")) <|>
        option
          auto
          (long "delay" <> value (50 :: Double) <>
           help "Delay between frames in ms")))
      empty
  initializeAll
  window <-
    createWindow
      (T.pack fp)
      defaultWindow {windowInitialSize = dim, windowHighDPI = True}
  renderer <- createRenderer window (-1) defaultRenderer
  -- Resize the window (smaller) if in high DPI (retina) mode.
  do Just (Rectangle _ (V2 w h)) <- SDL.get (SDL.rendererViewport renderer)
     let scale =
           let (V2 w0 h0) = dim
           in (V2
                 (fromIntegral w / fromIntegral w0 :: Double)
                 (fromIntegral h / fromIntegral h0 :: Double))
     windowSize window $= (fmap round (fmap fromIntegral dim / scale))
  -- Load up the sprite.
  sprite0 <- SDL.Sprite.load renderer fp dim
  -- Render the sprite on a timer.
  let loop (!sprite) = do
        events <- pollEvents
        let eventQuit event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                WindowClosedEvent {} -> True
                _ -> False
        clear renderer
        SDL.Sprite.render sprite (V2 0 0)
        present renderer
        threadDelay (round (1000 * ms))
        unless (any eventQuit events) (loop (SDL.Sprite.animate sprite))
  loop sprite0
