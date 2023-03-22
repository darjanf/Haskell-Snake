{-# Language FlexibleContexts #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Random (randomRIO)

import Debug.Trace

type Row       = Int
type Col       = Int
type Point     = (Int, Int)
type Size      = (Int, Int)
type Score     = Int
data Food      = Food  { getFood  :: Point   } deriving Show
data Snake     = Snake { getSnake :: [Point] } deriving Show
data Direction = U | R | D | L                 deriving Show

data Config = Config { cSize :: Size } deriving Show
data Game   = Game   { gSnake :: Snake, gFood :: Food, gDirection :: Direction, gScore :: Score } deriving Show

type World a = ReaderT Config (StateT Game IO) a

fromMaybe :: Maybe a -> a
fromMaybe (Just a) = a

getSize :: IO Size
getSize = fromMaybe <$> getTerminalSize

setup :: IO ()
setup = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor

giveFood :: MonadState Game m => m Point
giveFood  = gets (getFood . gFood)

giveSize :: MonadReader Config m => m Size
giveSize  = asks cSize

giveSnake :: MonadState Game m => m [Point]
giveSnake = gets (getSnake . gSnake)

spawnFood :: Size -> IO Food
spawnFood (mrow, mcol) = do
  frow <- randomRIO (1, mrow - 1)
  fcol <- randomRIO (1, mcol - 1)
  return (Food (frow, fcol))

mkGame :: IO Game
mkGame = do
  size <- getSize
  food <- spawnFood size
  return $ Game { gSnake = Snake (zip (repeat 10) [12 .. 17]), gFood = food, gScore = 0, gDirection = L }

mkConfig :: IO Config
mkConfig = do
  size <- getSize
  return (Config { cSize = size })

renderChar :: Char -> (Row, Col) -> IO ()
renderChar char (row, col) = do
  setCursorPosition row col
  putChar char

renderFood :: World ()
renderFood = do
  food <- giveFood
  liftIO $ renderChar '@' food

renderSnake :: World ()
renderSnake = do
  snake <- giveSnake
  liftIO $ do
    renderChar 'o' (head snake)
    mapM_ (renderChar 'x') (tail snake)

renderGame :: World ()
renderGame = do
  liftIO clearScreen
  renderSnake
  renderFood

moveHead :: Point -> Size -> Direction -> Point
moveHead (prow, pcol) (mrow, mcol) D = if prow == mrow - 1 then (1, pcol)        else (prow + 1, pcol)
moveHead (prow, pcol) (mrow, mcol) L = if pcol == 1        then (prow, mcol - 1) else (prow, pcol - 1)
moveHead (prow, pcol) (mrow, mcol) R = if pcol == mcol - 1 then (prow, 1)        else (prow, pcol + 1)
moveHead (prow, pcol) (mrow, mcol) U = if prow == 1        then (mrow - 1, pcol) else (prow - 1, pcol)

moveSnake :: (MonadReader Config m, MonadState Game m) => m ()
moveSnake = do
  oldSnake     <- giveSnake
  (mrow, mcol) <- giveSize
  direction    <- gets gDirection
  let newHead = moveHead (head oldSnake) (mrow, mcol) direction
      newBody = init oldSnake
  modify (\g -> g { gSnake = Snake (newHead : newBody) })

playGame :: World ()
playGame = do
  renderGame
  _ <- liftIO getChar
  moveSnake
  playGame

main :: IO ()
main = do
  setup
  config <- mkConfig
  game   <- mkGame
  runStateT (runReaderT playGame config) game
  _ <- getChar
  clearScreen
  showCursor
  return ()
