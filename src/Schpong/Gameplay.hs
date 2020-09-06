{- |
   Module      : Schpong.Gameplay
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Mechanics and rules of the game.
-}

module Schpong.Gameplay (
  Ball(..),
  Frame(..),
  Wall(..),
  initFrame,
  move
  ) where

import Schpong.Constants

-- | A point on the x-y plane (same as 'Point' from  Gloss).
type Point = (Float, Float)

data Ball = Ball Float Point Point
  deriving (Show)

data Wall = Wall Point Point
  deriving (Show)

data Contact = Bottom
             | Top
             | RightSide
             | LeftSide
             | None
  deriving (Show)

-- | State of the game for a given moment.
data Frame = Frame [Wall] [Ball] Float
  deriving (Show)

-- | Initial state of the game.
initFrame :: Frame
initFrame = Frame [bottom, right, up, left] [ball] 0
  where
    halfWidth = 0.5 * (fromIntegral windowWidth)
    halfHeight = 0.5 * (fromIntegral windowHeight)
    bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)
    right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)
    up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)
    left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)
    ball = Ball 10 (0, 0) (xspeed, 0)

-- | Move a ball (going up and down, and boucing against walls).
move :: Float -> [Wall] -> Ball -> Ball
move dt walls ball = bounce walls $ gravity dt ball

-- | Get the speed and position of a ball using Newton's law.
gravity :: Float -> Ball -> Ball
gravity dt (Ball r (x, y) (vx, vy)) = Ball r (x', y') (vx, vy')
  where
    x' = vx * dt + x
    y' = (-0.5) * g * (dt**2) + vy * dt + y
    vy' = (-g) * dt + vy

-- | Make a ball bounce against walls.
bounce :: [Wall] -> Ball -> Ball
bounce [] ball = ball
bounce (w:ws) (Ball r (x, y) (vx, vy)) = case getContact (Ball r (x, y) (vx, vy)) w of
  Bottom -> Ball r (x, y + eps) (vx, vy')
  Top -> Ball r (x, y - eps) (vx, 0)
  RightSide -> Ball r (x - eps, y) (-vx, vy)
  LeftSide -> Ball r (x + eps, y) (-vx, vy)
  None -> bounce ws (Ball r (x, y) (vx, vy))
  where
    vy' = max (abs vy) yBounceSpeed

-- | Return the (eventual) contact between a ball and a wall.
getContact :: Ball -> Wall -> Contact
getContact (Ball r (x, y) _) (Wall (x0, y0) (x1, y1))
  | x0 <= x && x <= x1 && y - r <= y1 && y1 <= y + r = Bottom
  | x0 <= x && x <= x1 && y - r <= y0 && y0 <= y + r = Top
  | x - r <= x0 && x0 <= x + r && y0 <= y && y <= y1 = RightSide
  | x - r <= x1 && x1 <= x + r && y0 <= y && y <= y1 = LeftSide
  | otherwise = None