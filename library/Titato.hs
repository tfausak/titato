module Titato where

import Data.Function ((&))

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified System.Random as Random

displayMode :: Gloss.Display
displayMode = let
    name = "Titato"
    width = 512
    height = 512
    size = (width, height)
    x = 0
    y = 0
    position = (x, y)
    in Gloss.InWindow name size position

backgroundColor :: Gloss.Color
backgroundColor = let
    red = 0x18
    green = 0x18
    blue = 0x18
    alpha = 0xff
    in Gloss.makeColorI red green blue alpha

stepsPerSecond :: Int
stepsPerSecond = 60

data Move
    = X
    | O
    deriving (Eq, Ord, Show)

type Moves = [[Maybe Move]]

data World = World
    { worldGenerator :: Random.StdGen
    , worldMoves :: Moves
    , worldSize :: (Int, Int)
    , worldStep :: Int
    , worldTime :: Float
    } deriving (Show)

clearMoves :: World -> World
clearMoves world = world
    { worldMoves = Nothing & replicate 3 & replicate 3
    }

initialWorld :: World
initialWorld = let
    Gloss.InWindow _name size _position = displayMode
    world = World
        { worldGenerator = Random.mkStdGen 0
        , worldMoves = []
        , worldSize = size
        , worldStep = 0
        , worldTime = 0
        }
    in clearMoves world

toViewPort :: World -> Gloss.ViewPort
toViewPort world = let
    (width, height) = worldSize world
    size = min width height
    scale = fromIntegral size / 2
    in Gloss.viewPortInit { Gloss.viewPortScale = scale }

third :: Float
third = 1 / 3

indexedMoves :: World -> [(Int, Int, Maybe Move)]
indexedMoves world = world
    & worldMoves
    & map (zip [0 ..])
    & zip [0 ..]
    & foldMap (\ (row, columns) ->
        foldMap (\ (column, move) -> [(row, column, move)]) columns)

toColor :: Move -> Gloss.Color
toColor move = let
    (red, green, blue) = case move of
        X -> (0x7c, 0xaf, 0xc2)
        O -> (0xdc, 0x96, 0x56)
    alpha = 0xff
    in Gloss.makeColorI red green blue alpha

drawMove :: Int -> Int -> Maybe Move -> Gloss.Picture
drawMove row column maybeMove = case maybeMove of
    Just move -> let
        radius = 0.9 * third
        x = (fromIntegral column - 1) * 2 * third
        y = -(fromIntegral row - 1) * 2 * third
        color = toColor move
        in Gloss.circleSolid radius & Gloss.translate x y & Gloss.color color
    _ -> Gloss.blank

gridColor :: Gloss.Color
gridColor = let
    red = 0x58
    green = 0x58
    blue = 0x58
    alpha = 0xff
    in Gloss.makeColorI red green blue alpha

drawWorld :: World -> Gloss.Picture
drawWorld world = let
    viewPort = toViewPort world
    grid = map (Gloss.color gridColor)
        [ Gloss.line [(-third, -1), (-third, 1)]
        , Gloss.line [(third, -1), (third, 1)]
        , Gloss.line [(-1, -third), (1, -third)]
        , Gloss.line [(-1, third), (1, third)]
        ]
    moves = world
        & indexedMoves
        & map (\ (row, column, move) -> drawMove row column move)
    pictures = grid ++ moves
    in pictures & Gloss.pictures & Gloss.applyViewPortToPicture viewPort

update :: Int -> (a -> a) -> [a] -> [a]
update n f xs = xs & zip [0 ..] & map (\ (i, x) -> if i == n then f x else x)

makeMove :: Int -> Int -> Move -> World -> World
makeMove row column move world = let
    moves = world & worldMoves & update row (update column (\ _ -> Just move))
    in world { worldMoves = moves }

choice :: (Random.RandomGen g) => [a] -> g -> Maybe (a, g)
choice list generator = case list of
    [] -> Nothing
    _ -> let
        size = length list
        (index, newGenerator) = Random.randomR (0, size - 1) generator
        element = list !! index
        in Just (element, newGenerator)

makeOpponentMove :: World -> World
makeOpponentMove world = let
    availableMoves = world
        & indexedMoves
        & filter (\ (_row, _column, maybeMove) -> Maybe.isNothing maybeMove)
    move = world & worldGenerator & choice availableMoves
    in case move of
        Just ((row, column, _move), newGenerator) -> world
            & makeMove row column O
            & (\ w -> w { worldGenerator = newGenerator })
        _ -> world

makePlayerMove :: Int -> Int -> World -> World
makePlayerMove row column world = let
    move = worldMoves world !! row !! column
    in case move of
        Nothing -> makeMove row column X world
        _ -> world

anyEnding :: Moves -> Bool
anyEnding moves = let
    p xs = xs
        & Maybe.catMaybes
        & List.sort
        & List.group
        & map length
        & any (== 3)
    in any p moves

isOver :: World -> Bool
isOver world = let
    moves = worldMoves world
    horizontal = anyEnding moves
    vertical = moves & List.transpose & anyEnding
    diagonal = anyEnding
        [ [moves !! 0 !! 0, moves !! 1 !! 1, moves !! 2 !! 2]
        , [moves !! 0 !! 2, moves !! 1 !! 1, moves !! 2 !! 0]
        ]
    in horizontal || vertical || diagonal

handleMove :: Gloss.Point -> World -> World
handleMove position world = if isOver world then clearMoves world else let
    viewPort = toViewPort world
    (x, y) = Gloss.invertViewPort viewPort position
    row = if y > third then 0 else if y < -third then 2 else 1
    column = if x < -third then 0 else if x > third then 2 else 1
    newWorld = makePlayerMove row column world
    in if isOver newWorld then newWorld else makeOpponentMove newWorld

handleEvent :: Gloss.Event -> World -> World
handleEvent event world = case event of
    Gloss.EventResize size -> world { worldSize = size }
    Gloss.EventKey key state _modifiers position -> case (key, state) of
        (Gloss.MouseButton Gloss.LeftButton, Gloss.Up) ->
            handleMove position world
        _ -> world
    _ -> world

handleStep :: Float -> World -> World
handleStep delta world = world
    { worldStep = worldStep world + 1
    , worldTime = worldTime world + delta
    }

main :: IO ()
main = do
    generator <- Random.getStdGen
    let world = initialWorld { worldGenerator = generator }
    Gloss.play
        displayMode
        backgroundColor
        stepsPerSecond
        world
        drawWorld
        handleEvent
        handleStep
