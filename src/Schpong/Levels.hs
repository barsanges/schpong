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
allLevels = [level01, level02, level03, level04, level05, level06]

halfWidth :: Float
halfWidth = 0.5 * (fromIntegral windowWidth)

halfHeight :: Float
halfHeight = 0.5 * (fromIntegral windowHeight)

bottom :: Wall
bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)

right :: Wall
right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)

up :: Wall
up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)

left :: Wall
left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)

level01 :: Frame
level01 = Frame [bottom, right, up, left] [ball] char Nothing
  where
    ball = Ball (2.1 * minBallRadius) (0, 0) (xspeed, 0)
    char = Character 0 Still

level02 :: Frame
level02 = Frame [bottom, right, up, left] [ball01, ball02] char Nothing
  where
    ball01 = Ball (4.1 * minBallRadius) (100, 0) (xspeed, 0)
    ball02 = Ball (4.1 * minBallRadius) (-100, 0) (-xspeed, 0)
    char = Character 0 Still

level03 :: Frame
level03 = Frame [bottom, right, up, left]
                [ball01, ball02, ball03, ball04]
                char
                Nothing
  where
    ball01 = Ball (2.1 * minBallRadius) (-300, floorLevel + 10) (xspeed, yBounceSpeed)
    ball02 = Ball (2.1 * minBallRadius) (-150, floorLevel + 10) (xspeed, yBounceSpeed)
    ball03 = Ball (2.1 * minBallRadius) (150, floorLevel + 10) (-xspeed, yBounceSpeed)
    ball04 = Ball (2.1 * minBallRadius) (300, floorLevel + 10) (-xspeed, yBounceSpeed)
    char = Character 0 Still

level04 :: Frame
level04 = Frame [bottom, right, up, left, middle01, middle02, middle03]
                [ball01, ball02, ball03, ball04]
                char
                Nothing
  where
    middle01 = Wall (-halfWidth / 2 - 5, -halfHeight + 100) (-halfWidth / 2 + 5, halfHeight)
    middle02 = Wall (-5, -halfHeight + 100) (5, halfHeight)
    middle03 = Wall (halfWidth / 2 - 5, -halfHeight + 100) (halfWidth / 2 + 5, halfHeight)
    ball01 = Ball (2.1 * minBallRadius) (-3 * halfWidth / 4, 0) (xspeed, yBounceSpeed)
    ball02 = Ball (2.1 * minBallRadius) (-halfWidth / 4, 0) (xspeed, yBounceSpeed)
    ball03 = Ball (2.1 * minBallRadius) (halfWidth / 4, 0) (xspeed, yBounceSpeed)
    ball04 = Ball (2.1 * minBallRadius) (3 * halfWidth / 4, 0) (xspeed, yBounceSpeed)
    char = Character (-halfWidth + 100) Still

level05 :: Frame
level05 = Frame [bottom, right, up, left, middle01, middle02, step01, step02, step03]
                [ball01, ball02, ball03]
                char
                Nothing
  where
    middle01 = Wall (-halfWidth / 3 - 5, 50) (-halfWidth / 3 + 5, halfHeight)
    middle02 = Wall (halfWidth / 3 - 5, 50) (halfWidth / 3 + 5, halfHeight)
    step01 = Wall (-halfWidth, -205) (-halfWidth / 3 - 50, -195)
    step02 = Wall (-halfWidth / 3 + 50, -205) (halfWidth / 3 - 50, -195)
    step03 = Wall (halfWidth / 3 + 50, -205) (halfWidth, -195)
    ball01 = Ball (2.1 * minBallRadius) (-halfWidth / 3 - 50, 0) (-xspeed, yBounceSpeed)
    ball02 = Ball (2.1 * minBallRadius) (-halfWidth / 3 + 50, 0) (xspeed, yBounceSpeed)
    ball03 = Ball (2.1 * minBallRadius) (halfWidth / 3 + 50, 0) (xspeed, yBounceSpeed)
    char = Character 0 Still

level06 :: Frame
level06 = Frame [bottom, right, up, left]
                [ball01, ball02, ball03, ball04, ball05]
                char
                Nothing
  where
    ball01 = Ball (4.1 * minBallRadius) (-4 * halfWidth / 5, halfHeight / 2) (0, 0)
    ball02 = Ball (4.1 * minBallRadius) (-2 * halfWidth / 5, halfHeight / 2) (0, 0)
    ball03 = Ball (4.1 * minBallRadius) (0, halfHeight / 2) (0, 0)
    ball04 = Ball (4.1 * minBallRadius) (2 * halfWidth / 5, halfHeight / 2) (0, 0)
    ball05 = Ball (4.1 * minBallRadius) (4 * halfWidth / 5, halfHeight / 2) (0, 0)
    char = Character 0 Still