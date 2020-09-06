{- |
   Module      : Schpong.Graphics
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Graphical interface for a game of Schpong.
-}

module Schpong.Graphics (
  runGame
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Schpong.Constants
import Schpong.Gameplay

-- | Run a game of Schpong.
runGame :: IO ()
runGame = play
  windowDisplay
  white
  stepsPerSecond
  initFrame
  drawFrame
  handle
  update

-- | Create a new window for the game.
windowDisplay :: Display
windowDisplay = InWindow "Schpong" (windowWidth, windowHeight) (10, 10)

-- | Turn a frame into a Gloss picture.
drawFrame :: Frame -> Picture
drawFrame (Frame walls balls) = pictures (pwalls ++ pballs)
  where
    pwalls = fmap drawWall walls
    pballs = fmap drawBall balls

-- | Turn a ball into a Gloss picture.
drawBall :: Ball -> Picture
drawBall (Ball r (x, y) _) = translate x y (Color red (circleSolid r))

-- | Turn a wall into a Gloss picture.
drawWall :: Wall -> Picture
drawWall (Wall (x0, y0) (x1, y1)) = translate x y (rectangleSolid width height)
  where
    width = x1 - x0
    height = y1 - y0
    x = (x0 + x1) / 2
    y = (y0 + y1) / 2

-- | Handle input events (e.g.: key press).
handle :: Event -> Frame -> Frame
handle _ x = x

-- | Update a frame.
update :: Float -> Frame -> Frame
update dt (Frame walls balls) = Frame walls (fmap (move dt walls) balls)