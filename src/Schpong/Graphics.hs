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
import Schpong.Levels

-- | The state of the game.
data GameState = Menu
               | Playing Frame
               | GameOver Frame
               | Victory Frame

-- | Run a game of Schpong.
runGame :: IO ()
runGame = play
  windowDisplay
  white
  stepsPerSecond
  Menu
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
drawGameState (Victory frame) = drawVictory $ drawFrame frame
drawGameState Menu = snd (mkMenu allLevels)

-- | Select a level on screen.
selectLevel :: Point -> Maybe Frame
selectLevel = fst (mkMenu allLevels)

-- | Make the menu.
mkMenu :: [Frame] -> ((Point -> Maybe Frame), Picture)
mkMenu frame = let (fs, pics) = unzip (zipWith toMenu [0..] frame)
               in (fstJust fs, Pictures pics)

-- | Set a level in the menu.
toMenu :: Int -> Frame -> ((Point -> Maybe Frame), Picture)
toMenu idx frame = (go, translate x0 y0 $ draw2Digits (show (idx + 1)))
  where
    a = 20 -- A bit more than the half-size of the square displayed on screen.
    ncols = 5
    row = idx `div` ncols
    col = idx `mod` ncols
    x0 = -300 + 150 * (fromIntegral col)
    y0 = 200 - 200 * (fromIntegral row)
    go (x, y) = if abs (x - x0) < a && abs (y - y0) < a
                then Just frame
                else Nothing

-- | Apply the functions of the list to the parameter and return the first
-- result that is not 'Nothing'.
fstJust :: [a -> Maybe b] -> a -> Maybe b
fstJust [] _ = Nothing
fstJust (f:fs) x = case f x of
  Nothing -> fstJust fs x
  Just y -> Just y

-- | Draw the given string (two digits expected) in a square.
draw2Digits :: String -> Picture
draw2Digits str = Pictures [ translate 11.5 11.5 $ rectangleWire 38 38
                           , scale 0.2 0.2 $ text str ]

-- | Add a "Game over" text to a picture.
drawGameOver :: Picture -> Picture
drawGameOver p = Pictures [ Translate (-175) 100 $ Text "Game"
                          , Translate (-125) (-100) $ Text "Over"
                          , p ]

-- | Add a "Victory!" text to a picture.
drawVictory :: Picture -> Picture
drawVictory p = Pictures [ Translate (-175) 0 $ Text "Victory!"
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
drawBall (Ball r (x, y) _) = translate x y (Color (ballColor r) (circleSolid r))

-- | Get the color of a ball, depending on its radius.
ballColor :: Float -> Color
ballColor r
  | 4.5 * minBallRadius <= r = green
  | 2.5 * minBallRadius <= r && r <= 4.5 * minBallRadius = blue
  | 1.5 * minBallRadius <= r && r <= 2.5 * minBallRadius = red
  | otherwise = yellow

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
drawRope (Just (x, top)) = translate x y (Color (greyN 0.2) (rectangleSolid ropeWidth height))
  where
    y = (top + floorLevel) / 2
    height = top - floorLevel

-- | Handle input events (e.g.: key press).
handle :: Event -> GameState -> GameState
handle (EventKey (MouseButton LeftButton) Down _ point) Menu = case selectLevel point of
  Just frame -> Playing frame
  Nothing -> Menu
handle _ Menu = Menu
handle (EventKey (SpecialKey KeySpace) Down _ _) (GameOver _) = Menu
handle _ (GameOver f) = GameOver f
handle (EventKey (SpecialKey KeySpace) Down _ _) (Victory _) = Menu
handle _ (Victory f) = Victory f
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
update _ Menu = Menu
update _ (GameOver f) = GameOver f
update _ (Victory f) = Victory f
update dt (Playing (Frame walls [] x rope)) = Victory (Frame walls [] x rope)
update dt (Playing (Frame walls balls x rope)) = case hit x' balls' of
  True -> GameOver f'
  False -> Playing f'
  where
    x' = walk dt walls x
    balls' = fmap (move dt walls) balls
    rope' = throw dt rope
    (balls'', rope'') = cut walls balls' rope'
    f' = Frame walls balls'' x' rope''