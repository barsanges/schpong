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

-- | The state of the game.
data GameState = Playing Frame
               | GameOver Frame

-- | Run a game of Schpong.
runGame :: IO ()
runGame = play
  windowDisplay
  white
  stepsPerSecond
  (Playing initFrame)
  drawGameState
  handle
  update

-- | Create a new window for the game.
windowDisplay :: Display
windowDisplay = InWindow "Schpong" (windowWidth, windowHeight) (10, 10)

-- | Turn a game state into a Gloss picture.
drawGameState :: GameState -> Picture
drawGameState (Playing frame) = drawFrame frame
drawGameState (GameOver frame) = drawGameOver $ drawFrame frame

-- | Add a "Game over" text to a picture.
drawGameOver :: Picture -> Picture
drawGameOver p = Pictures [ Translate (-175) 100 $ Text "Game"
                          , Translate (-125) (-100) $ Text "Over"
                          , p ]

-- | Turn a frame into a Gloss picture.
drawFrame :: Frame -> Picture
drawFrame (Frame walls balls char rope) = pictures (prope:pchar:(pwalls ++ pballs))
  where
    pwalls = fmap drawWall walls
    pballs = fmap drawBall balls
    pchar = drawCharacter char
    prope = drawRope rope

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

-- | Turn the character into a Gloss picture.
drawCharacter :: Character -> Picture
drawCharacter (Character x _) =
  translate x y (Color (greyN 0.4) (rectangleSolid width height))
  where
    (width, height) = characterSize
    y = 10 - 0.5 * (fromIntegral windowHeight) + 0.5 * height

-- | Turn the rope into a Gloss picture.
drawRope :: Maybe Point -> Picture
drawRope Nothing = Blank
drawRope (Just (x, h)) = translate x y (Color (greyN 0.2) (rectangleSolid ropeWidth height))
  where
    y = (h + floorLevel) / 2
    height = h - floorLevel

-- | Handle input events (e.g.: key press).
handle :: Event -> GameState -> GameState
handle _ (GameOver f) = GameOver f
handle (EventKey (SpecialKey KeySpace) Down _ _)  (Playing (Frame walls balls (Character x dir) _)) =
  Playing $ Frame walls balls (Character x dir) (Just (x, floorLevel))
handle (EventKey (SpecialKey KeyLeft) Down _ _)  (Playing (Frame walls balls (Character x _) rope)) =
  Playing $ Frame walls balls (Character x ToLeft) rope
handle (EventKey (SpecialKey KeyLeft) Up _ _)  (Playing (Frame walls balls (Character x _) rope)) =
  Playing $ Frame walls balls (Character x Still) rope
handle (EventKey (SpecialKey KeyRight) Down _ _)  (Playing (Frame walls balls (Character x _) rope)) =
  Playing $ Frame walls balls (Character x ToRight) rope
handle (EventKey (SpecialKey KeyRight) Up _ _)  (Playing (Frame walls balls (Character x _) rope)) =
  Playing $ Frame walls balls (Character x Still) rope
handle _ (Playing f) = Playing f

-- | Update a frame.
update :: Float -> GameState -> GameState
update _ (GameOver f) = GameOver f
update dt (Playing (Frame walls balls x rope)) = case hit x' balls' of
  True -> GameOver f'
  False -> Playing f'
  where
    x' = walk dt walls x
    balls' = fmap (move dt walls) balls
    rope' = throw dt rope
    f' = Frame walls balls' x' rope'