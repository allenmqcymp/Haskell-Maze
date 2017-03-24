-- module Main where

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
{-
Remarks:  - TH Code = Tony Hosking Code
		  - M Code  = My own code
Author:   - Yuanfang (Allen) Ma
-}
-- main function at bottom!
-- Lists
-- TH Code
-- defines a list data type which we use to store the coordinates of boxes
-- a is polymorphic, but we use List Coord to store coordinates of boxes
data List a = Empty | Entry a (List a)
-- standard "map" function
-- takes a function that does an operation eg. check type of tile, check Coord and
-- applies function successively to elements of list, returning a new list with all elements
-- transformed by function
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)
-- combines pictures in a list together
combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps
-- Coordinates
-- TH Code
data Coord = C Integer Integer
data Direction = R | U | L | D
-- M Code
-- checks if two coordinates are the same
-- by checking each individual component of the coordinate
eqCoord :: Coord -> Coord -> Bool
(C x y) `eqCoord`  (C a b)
  | (x == a) && (y == b) = True
  | otherwise            = False
-- return the adjacent coordinate
-- adjacent coordinate are coordinates to the right, left, up, and down of
-- the current coordinate passed as the argument
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)
-- M Code
-- one coordinate further than the Coord adjacentCoord returns
beyondCoord :: Direction -> Coord -> Coord
beyondCoord R (C x y) = C (x+2) y
beyondCoord U (C x y) = C  x   (y+2)
beyondCoord L (C x y) = C (x-2) y
beyondCoord D (C x y) = C  x   (y-2)
-- The maze
-- TH Code
data Tile = Wall | Ground | Storage | Box | Blank
  deriving Eq
maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4    = Blank
  | abs x == 4 || abs y == 4   = Wall
  | x == 2 && y < 0            = Wall
  | x == 3 && y < 0            = Storage
  | x > -3 && x < 1 && y == 1  = Box
  | otherwise                  = Ground
-- M Code
-- if tile at Coordinate c is a Box, then return Ground
-- otherwise, just return what maze c would have returned
mazeWithoutBoxes :: Coord -> Tile
mazeWithoutBoxes c = case maze c of
    Box -> Ground
    _   -> maze c
-- M Code
-- checks if the coordinate is in the list (of box coordinates)
-- if True, return Box
-- otherwise, just do what mazeWithoutBoxes would do
-- because boxes constantly change coordinates, we need mazeWithBoxes to check whether
-- Coord we are moving into is a box or not
mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = mazeWithoutBoxes c
mazeWithBoxes lcs c = case elemCompare lcs c of
    True  -> Box
    False -> mazeWithoutBoxes c
-- M Code
-- traverses a list and checks whether an element is IN the list
-- used in mazeWithBoxes as a helper function
-- if yes, return True
-- if no, return False
elemCompare :: List Coord -> Coord -> Bool
elemCompare Empty _        = False
elemCompare (Entry x xs) c = case c `eqCoord` x of
    False -> elemCompare xs c
    _     -> True
-- The state
-- TH Code
-- state is the "world" in interactionOf while game is running
data State = St Coord Direction (List Coord)
-- M Code
-- in this case List a: a is parameterized by Coord
-- find all the coordinates (in maze function) that are boxes and put them in a list
boxes :: List Coord
boxes = traverseMaze (\r -> traverseMaze (\c -> checkBox r c Empty))
-- M Code
-- check if a box is in coordinate r c
-- if yes, then prepend to list, otherwise, just skip it and retain the list
checkBox :: Integer -> Integer -> List Coord -> List Coord
checkBox r c acc = case maze (C r c) of
    -- if a box is encountered prepend it to the list of boxes
    Box -> (Entry (C r c) acc)
    _   -> acc
-- M Code
-- go is a helper function that calls checkBox 11 times (size of each dimension of maze)
-- two lambda functions ensure that every coordinate (both dimensions are accounted for)
-- is checked for existence of box
traverseMaze :: (Integer -> List Coord) -> List Coord
traverseMaze something = go (-10)
    where
      go :: Integer -> List Coord
      go 11 = Empty
      go n  = appendList (something n) (go (n+1))
-- M Code
-- appends two lists together, one after the other
appendList :: List a -> List a -> List a
appendList Empty        Empty        = Empty
appendList Empty        (Entry x xs) = (Entry x xs)
appendList (Entry x xs) list2        = (Entry x (appendList xs list2))
-- the initial state of the player
-- arbitrary direction R and (C 0 0) chosen because it's a ground
-- boxes is function that gives the list of coords of boxes
-- contains the boxes as described in the maze function found by traverseMaze function
initialState :: State
initialState = St (C 0 0) R boxes
-- Event Handling
-- M Code
-- first check if game is won or not
-- if not, then proceed with normal event handling
-- otherwise, return state unaltered
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (St c d l) =
  case gameWon l of
    True  -> state
    False -> case key of
                "Right" -> tryMove state R
                "Left" -> tryMove state L
                "Up"   -> tryMove state U
                "Down"  -> tryMove state D
                _       -> state
  where state = St c d l
handleEvent _ state = state
-- M Code
-- first check if the coord player moves to is occupied by a box
-- if yes, check the beyond coordinate with respect to player
-- that is, the adjacent coordinate of the box
-- if possible to move there, then update coord of boxes using movedBoxes function
-- if not, don't let the player move
-- if coord player moves to isn't a box, then determine if it's ground or storage
-- for a legal move
tryMove :: State -> Direction -> State
tryMove (St from _ lcs) cd =
  case mazeWithBoxes lcs to of
    Box      -> case mazeWithBoxes lcs beyond of
                    Storage -> St to cd movedBoxes
                    Ground  -> St to cd movedBoxes
                    _       -> St from cd lcs
                where movedBoxes = mapList (moveFromTo to beyond) lcs
    Ground   -> St to cd lcs
    Storage  -> St to cd lcs
    _        -> St from cd lcs
  where to     = adjacentCoord cd from
        beyond = beyondCoord cd from
-- Box moving handling
-- TH Code
-- `moveFromTo` updates the coordinates (of boxes)
moveFromTo :: Coord -> Coord -> (Coord -> Coord)
moveFromTo from to currentBox | from `eqCoord` currentBox = to
                              | otherwise          = currentBox
-- Winning the game!
-- M Code
-- if every coordinate is on a storage tile then gameWon is True
gameWon :: List Coord -> Bool
-- if there exists no box, then the game is by default won (defined for completeness)
-- (kind of redundant, because existence of boxes is hardcoded into maze function)
gameWon Empty        = True
gameWon (Entry x xs) = allList (mapList isOnStorage (Entry x xs))
-- if the given coordinate corresponds to a storage, then return True, else False
isOnStorage :: Coord -> Bool
isOnStorage c
  | (mazeWithoutBoxes c) == Storage = True
  | otherwise                       = False
-- if every entry in the list is Storage then return True
-- else False; empty List returns true
-- works by doing logical AND
-- since AND returns True iff every single element is True
allList :: List Bool -> Bool
allList list = case list of
    Empty             -> True
    (Entry x xs)      -> (x && True) && allList xs
-- Drawing
-- TH Code
-- describes how to draw each type of tile
wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white      (solidCircle    0.3) & ground
box =     colored brown      (solidRectangle 1 1)
-- draws appropriate tile corresponding to type given
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank
-- draws maze in similar vein to traverseMaze
-- two lambda functions handle every coordinate
-- drawTile is responsible for drawing at coordinates given from lambda functions
pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))
-- go is a helper function that iterates 11 times and stops
draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)
-- draws appropriate tile at coord c
-- does not draw boxes
drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (mazeWithoutBoxes c))
-- draws player at coordinate c
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic
-- describes how to draw player in all 4 orientations
player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(-0.3,0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)]
         & path [(0,0),(-0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)
-- TH Code
-- draws picture of boxes based on current coordinates in list - lcs
-- combines pictures together
-- each "frame", the all boxes in their locations are redrawn
pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes lcs = combine (mapList (\c -> atCoord c (drawTile Box)) lcs)
-- M Code
-- passed into interactionOf
-- draws boxes, maze (without boxes) and player (and orientation)
-- if game is won, then also display text "You Won!"
drawState :: State -> Picture
drawState (St c d l) =
  case gameWon l of
    True  -> (scaled 2 2 (text "You Won!")) & entireGame
    False -> entireGame
  where entireGame = (atCoord c (player d)) & (pictureOfBoxes l) & (pictureOfMaze)
-- The general interaction type
data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)
runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw
-- The complete interaction
-- TH Code
-- all the actual parameters to runInteraction are defined here
-- initialState
-- handleTime (replaced by lambda function)
-- handleEvent
-- drawState
warehouse :: Interaction State
warehouse = Interaction initialState (\_ c -> c) handleEvent drawState
-- Resetable interactions
-- TH Code
-- when the Esc key is pressed at any time, because resetable is composed on withStartScreen
-- the state0 of withStartScreen is state0' which is StartScreen (defined in withStartScreen)
-- thus we return back to the StartScreen type
-- if anything other than Esc is pressed, then resetable hands over control to handleEvent
resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s
-- Start screen
-- TH Code
startScreen :: Picture
startScreen = scaled 3 3 (text "Warehouse!")
data SSState world = StartScreen | Running world
-- function that controls whether it's startScreen or Running
-- local definitions are used, taking advantages of interactionOf's
-- polymorphic data type world, so that locally, we can define world as
-- a StartScreen type
-- then hand over control to handleEvent and drawState which take State as
-- the world type
-- when the spacebar key is pressed
withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    draw' StartScreen = startScreen
    -- else, hand over control to drawState (drawState is the parameter to draw)
    draw' (Running s) = draw s
-- The main function
-- entry point
main :: IO ()
main = runInteraction (resetable (withStartScreen warehouse))