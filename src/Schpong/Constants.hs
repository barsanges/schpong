{- |
   Module      : Schpong.Constants
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Constants used within the game.
-}

module Schpong.Constants (
  characterSize,
  eps,
  g,
  stepsPerSecond,
  windowHeight,
  windowWidth,
  xspeed,
  yBounceSpeed
  ) where

-- | Size of the character.
characterSize :: (Float, Float)
characterSize = (10, 30)

-- | Speed of a ball on the x axis (= distance for one second).
xspeed :: Float
xspeed = 80

-- | Minimal speed of a ball on the y axis (= distance for one second) after
-- bouncing on the floor.
yBounceSpeed :: Float
yBounceSpeed = 100

-- | Gravitational constant.
g :: Float
g = 60

-- | Small distance on screen.
eps :: Float
eps = 1

-- | Number of simulation steps to take for each second of real time.
stepsPerSecond :: Int
stepsPerSecond = 60

-- | Height of the game window.
windowHeight :: Int
windowHeight = 600

-- | Width of the game window.
windowWidth :: Int
windowWidth = 800