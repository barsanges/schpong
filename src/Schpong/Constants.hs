{- |
   Module      : Schpong.Constants
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Constants used within the game.
-}

module Schpong.Constants (
  characterSize,
  characterSpeed,
  eps,
  floorLevel,
  g,
  minBallRadius,
  ropeSpeed,
  ropeWidth,
  stepsPerSecond,
  windowHeight,
  windowWidth,
  xspeed,
  yBounceSpeed
  ) where

-- | Width of the rope.
ropeWidth :: Float
ropeWidth = 2

-- | Speed of the rope on the y axis.
ropeSpeed :: Float
ropeSpeed = 4 * yBounceSpeed

-- | Size of the character.
characterSize :: (Float, Float)
characterSize = (30, 50)

-- | Speed of the character on the x axis.
characterSpeed :: Float
characterSpeed = 2.5 * xspeed

-- | Minimal radius of a ball.
minBallRadius :: Float
minBallRadius = 10

-- | Speed of a ball on the x axis (= distance for one second).
xspeed :: Float
xspeed = 120

-- | Minimal speed of a ball on the y axis (= distance for one second) after
-- bouncing on the floor.
yBounceSpeed :: Float
yBounceSpeed = 200

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

-- | Ordinate of the floor on which the character is walking.
floorLevel :: Float
floorLevel = 10 - 0.5 * (fromIntegral windowHeight)