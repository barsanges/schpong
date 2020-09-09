{- |
   Module      : Schpong.Gameplay
   Copyright   : Copyright (C) 2020 barsanges
   License     : Non-Profit Open Software License 3.0

Mechanics and rules of the game.
-}

module Schpong.Gameplay (
  Ball(..),
  Character(..),
  Frame(..),
  Movement(..),
  Wall(..),
  cut,
  initFrame,
  hit,
  move,
  throw,
  walk
  ) where

import Data.Maybe (catMaybes)
import Data.List (partition)
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
  deriving (Show, Eq)

data Movement = ToLeft
              | ToRight
              | Still
  deriving (Show)

data Character = Character Float Movement
  deriving (Show)

-- | State of the game for a given moment.
data Frame = Frame [Wall] [Ball] Character (Maybe Point)
  deriving (Show)

-- | Initial state of the game.
initFrame :: Frame
initFrame = Frame [bottom, right, up, left] [ball] char Nothing
  where
    halfWidth = 0.5 * (fromIntegral windowWidth)
    halfHeight = 0.5 * (fromIntegral windowHeight)
    bottom = Wall (-halfWidth, -halfHeight) (halfWidth, -halfHeight + 10)
    right = Wall (halfWidth - 10, -halfHeight) (halfWidth, halfHeight)
    up = Wall (-halfWidth, halfHeight - 10) (halfWidth, halfHeight)
    left = Wall (-halfWidth, -halfHeight) (-halfWidth + 10, halfHeight)
    ball = Ball 40 (0, 0) (xspeed, 0)
    char = Character 0 Still

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

-- | Let the character walk on the floor, from left to right or from right to
-- left.
walk :: Float -> [Wall] -> Character -> Character
walk dt walls (Character x movmt) = case movmt of
  Still -> Character x movmt
  ToLeft -> Character (walk' x (x - dt * characterSpeed) walls) movmt
  ToRight -> Character (walk' x (x + dt * characterSpeed) walls) movmt

-- | Move the character between two points on the floor.
walk' :: Float -> Float -> [Wall] -> Float
walk' xa xb walls = if xa < xb'
                    then minimum collisions
                    else maximum collisions
  where
    xb' = walkWhileFloor xa xb walls
    collisions = fmap (collision xa xb') walls

-- | Move the character between two points, provided there is a floor.
walkWhileFloor :: Float -> Float -> [Wall] -> Float
walkWhileFloor xa xb walls = comp xb (foldr go xa walls')
 where
   isFloor (Wall _ (_, y)) = abs (y - floorLevel) < (1e-9)
   walls' = filter isFloor walls
   (comp, choose) = if xa < xb
                    then (min, max)
                    else (max, min)
   go :: Wall -> Float -> Float
   go (Wall (x0, _) (x1, _)) x = if x0 <= x && x <= x1
                                 then choose x0 x1
                                 else x

-- | Move the character between two points, provided the given wall does not
-- stand in the way.
collision :: Float -> Float -> Wall -> Float
collision xa xb (Wall (x0, y0) (x1, y1))
  | y0 > floorLevel + height || y1 <= floorLevel || sideTest = xb
  | otherwise = xa
  where
    sideTest = if xa < xb
               then (xb + 0.5 * width < x0) || x1 <= xa
               else (xb - 0.5 * width > x1) || x0 >= xa
    (width, height) = characterSize

-- | Detect if a ball is hitting the character.
hit :: Character -> [Ball] -> Bool
hit (Character x0 _) = any go
  where
    (width, height) = characterSize
    go (Ball r (x, y) _) = side || top || leftCorner || rightCorner
      where
        side = abs (x0 - x) < r + 0.5 * width && y < floorLevel + height
        top = abs (x0 - x) < 0.5 * width && y - r < floorLevel + height
        leftCorner = sqrt ((x0 - 0.5 * width - x)**2 + (y - floorLevel - height)**2) < r
        rightCorner = sqrt ((x0 + 0.5 * width - x)**2 + (y - floorLevel - height)**2) < r

-- | Throw the character's rope in the air.
throw :: Float -> Maybe Point -> Maybe Point
throw _ Nothing = Nothing
throw dt (Just (x, y)) = if y' < - floorLevel
                         then Just (x, y')
                         else Nothing
  where
    y' = y + dt * ropeSpeed

-- | Cut in two the balls that meet the character's rope.
cut :: [Wall] -> [Ball] -> Maybe Point -> ([Ball], Maybe Point)
cut _ balls Nothing = (balls, Nothing)
cut walls balls (Just (x, h)) = (balls', rope')
  where
    ropeAsWall = Wall (x - 0.5 * ropeWidth, floorLevel) (x + 0.5 * ropeWidth, floorLevel + h)
    (contacts, others) = partition (\ b -> getContact b ropeAsWall /= None) balls
    contacts' = splitAll walls contacts
    balls' = contacts' ++ others
    rope' = if null contacts
            then Just (x, h)
            else Nothing

-- | Split all given balls and ensure that they do not get stuck in walls.
splitAll :: [Wall] -> [Ball] -> [Ball]
splitAll walls = flatten . catMaybes . (fmap (split walls))

-- | Split a ball in two, and ensure that the resulting balls do not get stuck
-- in walls.
split :: [Wall] -> Ball -> Maybe (Ball, Ball)
split walls (Ball r (x, y) _ )= if r' >= minBallRadius
                                then Just (ballLeft, ballRight)
                                else Nothing
  where
    r' = r / 2
    ballLeft = bounce walls (Ball r' (x - r' - 5, y) (-xspeed, 0))
    ballRight = bounce walls (Ball r' (x + r' + 5, y) (xspeed, 0))

flatten :: [(a, a)] -> [a]
flatten [] = []
flatten ((x1, x2):xs) = (x1:x2:flatten xs)