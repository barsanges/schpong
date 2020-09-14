{- |
   Module      : Schpong.Levels
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Levels of the game.
-}

module Schpong.Levels (
  allLevels
  ) where

import Schpong.Constants
import Schpong.Gameplay

allLevels :: [Frame]
allLevels = [level01, level02, level03]

level01 :: Frame
level01 = Frame [bottom, right, up, left] [ball] char Nothing
  where
    halfWidth = 0.5 * (fromIntegral windowWidth)
    halfHeight = 0.5 * (fromIntegral windowHeight)
    bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)
    right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)
    up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)
    left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)
    ball = Ball (2.1 * minBallRadius) (0, 0) (xspeed, 0)
    char = Character 0 Still

level02 :: Frame
level02 = Frame [bottom, right, up, left] [ball01, ball02] char Nothing
  where
    halfWidth = 0.5 * (fromIntegral windowWidth)
    halfHeight = 0.5 * (fromIntegral windowHeight)
    bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)
    right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)
    up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)
    left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)
    ball01 = Ball (4.1 * minBallRadius) (100, 0) (xspeed, 0)
    ball02 = Ball (4.1 * minBallRadius) (-100, 0) (-xspeed, 0)
    char = Character 0 Still

level03 :: Frame
level03 = Frame [bottom, right, up, left] [ball01, ball02, ball03, ball04] char Nothing
  where
    halfWidth = 0.5 * (fromIntegral windowWidth)
    halfHeight = 0.5 * (fromIntegral windowHeight)
    bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)
    right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)
    up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)
    left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)
    ball01 = Ball (2.1 * minBallRadius) (-300, floorLevel + 10) (xspeed, yBounceSpeed)
    ball02 = Ball (2.1 * minBallRadius) (-150, floorLevel + 10) (xspeed, yBounceSpeed)
    ball03 = Ball (2.1 * minBallRadius) (150, floorLevel + 10) (-xspeed, yBounceSpeed)
    ball04 = Ball (2.1 * minBallRadius) (300, floorLevel + 10) (-xspeed, yBounceSpeed)
    char = Character 0 Still