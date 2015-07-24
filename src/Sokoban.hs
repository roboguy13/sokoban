-- Date: July 9, 2015
{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

import qualified Data.Map as Map
import           Data.Map (Map)

import           Control.Monad.State

import           Control.Lens

import           Data.List
import           Data.Ord

import           System.Exit
import           System.IO

import           System.Console.ANSI

data GoalStatus = Open | Claimed | PlayerOnGoal deriving (Eq, Show)

data Tile
  = Player
  | Floor
  | Box
  | Goal GoalStatus
  | Wall
  deriving (Eq, Show)

makePrisms ''Tile

data Direction = U | D | L | R

type Position  = (Int, Int)
type GameState = Map Position Tile
type Game      = StateT GameState IO

claimed :: GoalStatus -> Bool
claimed Claimed = True
claimed _       = False

goalStatuses :: Game [GoalStatus]
goalStatuses = toListOf (traverse . _Goal) <$> get

finished :: Game Bool
finished = all claimed <$> goalStatuses

inBounds :: Position -> Bool
inBounds (x, y) = x >= 0 && y >= 0

hasPlayer :: Tile -> Bool
hasPlayer Player              = True
hasPlayer (Goal PlayerOnGoal) = True
hasPlayer _                   = False

playerPosition :: Game Position
playerPosition = do
  mPos <- fmap fst . find (hasPlayer . snd) . Map.assocs <$> get
  case mPos of
      Just pos -> return pos
      _        -> error "Internal error: Player not found"

movePlayer :: Direction -> Game ()
movePlayer dir = do
  currPos <- playerPosition
  let newPosition = move dir currPos

  when (inBounds newPosition) $ do
    Just tile <- use $ at newPosition
    case tile of
      Box          -> moveBox currPos newPosition
      Goal Claimed -> moveBox currPos newPosition
      Wall         -> return ()
      _            -> changePlayerPos currPos newPosition
  where
    moveBox :: Position -> Position -> Game ()
    moveBox oldPlayerPos position = do
      let newBoxPosition = move dir position

      when (inBounds newBoxPosition) $ do
        blocked <- isBlocked newBoxPosition
        unless blocked $ do
          changeBoxPos    position     newBoxPosition
          changePlayerPos oldPlayerPos position

changePlayerPos :: Position -> Position -> Game ()
changePlayerPos oldPos newPos = do
  deletePlayer oldPos
  ix newPos %= \case
    Goal Open -> Goal PlayerOnGoal
    _         -> Player

changeBoxPos :: Position -> Position -> Game ()
changeBoxPos oldPos newPos = do
  deleteBox oldPos
  ix newPos %= \case
    Goal Open -> Goal Claimed
    Floor     -> Box
    _         -> error "Internal error: this should never happen"

deletePlayer :: Position -> Game ()
deletePlayer position =
  ix position %= \case
    Player            -> Floor
    Goal PlayerOnGoal -> Goal Open
    x                 -> x -- This should never happen

deleteBox :: Position -> Game ()
deleteBox position =
  ix position %= \case
    Box          -> Floor
    Goal Claimed -> Goal Open
    x            -> x -- This should never happen

isBlocked :: Position -> Game Bool
isBlocked position = do
  Just tile <- use $ at position
  case tile of
    Floor     -> return False
    Goal Open -> return False
    _         -> return True

move :: Direction -> Position -> Position
move dir (x, y)
  = case dir of
      U -> (x,   y-1)
      D -> (x,   y+1)
      L -> (x-1, y  )
      R -> (x+1, y  )

readInput :: Game Direction
readInput = do
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  c <- liftIO getChar
  case c of
    'k' -> return U
    'j' -> return D
    'h' -> return L
    'l' -> return R
    'q' -> do
      liftIO $ putStr "\nAre you sure you want to quit (type y for yes): "
      liftIO $ hFlush stdout
      r <- liftIO $ getChar
      if r == 'y'
        then liftIO $ putStrLn "" >> exitSuccess
        else do
          displayGame
          readInput
    _   -> do
      liftIO $ putStrLn "\nInvalid input."
      readInput

-- TODO: Make this more readable
mapTo2DList :: Map Position a -> [[a]]
mapTo2DList = go . sortBy (comparing (swap . fst)) . Map.assocs
  where
    swap (a, b) = (b, a)

    go (((_, y1), a) : b@((_, y2), _) : rest)
      | y2 > y1    = [a] : go (b:rest)
    go ((_, x):xs) = _head %~ (x:) $ go xs
    go []          = [[]]

displayGame :: Game ()
displayGame = do
  liftIO clearFromCursorToScreenBeginning
  liftIO $ setCursorPosition 0 0

  listBoard <- use (to mapTo2DList)
  liftIO . putStrLn $ go listBoard
  liftIO $ putStrLn movementInstructions
  where
    go :: [[Tile]] -> String
    go ((tile:restRow):restBoard) = displayTile tile : go (restRow:restBoard)
    go ([]            :restBoard) = '\n'             : go restBoard
    go []                         = ""

    displayTile Player              = '@'
    displayTile Box                 = 'o'
    displayTile (Goal Open)         = '.'
    displayTile (Goal Claimed)      = '*'
    displayTile (Goal PlayerOnGoal) = '+'
    displayTile Floor               = ' '
    displayTile Wall                = '#'

read2DList :: [[a]] -> Map Position a
read2DList
  = Map.fromList
  . concatMap (\(y, xs) -> map (\(x, e) -> ((x, y), e)) xs)
  . zip [0..]
  . map (zip [0..])

readGameState :: [String] -> GameState
readGameState = read2DList . map (map readTile)

readTile :: Char -> Tile
readTile '@' = Player
readTile '+' = Goal PlayerOnGoal
readTile 'o' = Box
readTile '.' = Goal Open
readTile '*' = Goal Claimed
readTile ' ' = Floor
readTile '#' = Wall
readTile c   = error $ "Invalid tile: " ++ [c]

checkForWin :: Game ()
checkForWin = do
  cond <- finished
  when cond $ do
    displayGame
    liftIO $ putStrLn "You won!" >> exitSuccess

gameLoop :: Game ()
gameLoop = forever $ do
  displayGame
  readInput >>= movePlayer
  checkForWin


testBoard :: [String]
testBoard =
  ["#####"
  ,"#.o@#"
  ,"#####"
  ]

testBoard2 :: [String]
testBoard2 =
  ["    #####"
  ,"    #   #"
  ,"    #o  #"
  ,"  ###  o##"
  ,"  #  o o #"
  ,"### # ## #   ######"
  ,"#   # ## #####  ..#"
  ,"# o  o          ..#"
  ,"##### ### #@##  ..#"
  ,"    #     #########"
  ,"    #######"
  ]

movementInstructions :: String
movementInstructions
  = concat
  . intersperse "\n"
  $ map ("    " ++)
        ["Controls:"
        ,"   k  "
        ," h   l"
        ,"   j  "
        ,"        q: Quit"
        ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  evalStateT gameLoop $ readGameState testBoard2

