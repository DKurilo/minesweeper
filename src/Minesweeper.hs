{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Minesweeper
    ( Game(..)
    , Counter(..)
    , counterToInt
    , width
    , height
    , board
    , minesCount
    , minesTotal
    , mines
    , playTime
    , Cell(..)
    , Result(..)
    , Pos(..)
    , initGame
    , setMines
    , openCell
    , invertMine
    , invertSuspicious
    , discover
    , checkResult
    , tick
    , revealMines
    , foundMines
    ) where

import           Control.Lens    (makeLenses, view, (%~), (&), (+~), (.~), (^.))
import           Control.Monad   (foldM)
import           Data.Coerce     (coerce)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Text       (Text, words)
import           Options         (Options, defaultConfig)
import qualified Options         as O
import           Options.Generic (Unwrapped, unwrapRecordPure)
import           Prelude         hiding (words)
import           System.Random   (randomRIO)

type Pos = (Int, Int)

data Result = Winner | Loser | Playing

data Cell = Closed | Empty | Suspicious | Mine | Around Int | BOOM | HiddenMine | WrongMine
    deriving (Eq, Show)

newtype Counter = C Int
    deriving (Eq, Ord, Num, Show, Read) via Int

counterToInt :: Counter -> Int
counterToInt = coerce

instance Semigroup Counter where
    (<>) = (+)

instance Monoid Counter where
    mempty = 0

data Game = Game { _width      :: Int
                 , _height     :: Int
                 , _minesCount :: Counter
                 , _minesTotal :: Int
                 , _mines      :: S.Set Pos
                 , _board      :: M.Map Pos Cell
                 , _playTime   :: Counter
                 }
          | NotStarted { _width      :: Int
                       , _height     :: Int
                       , _minesTotal :: Int
                       }
makeLenses ''Game

pickRandom :: [a] -> IO (a, [a])
pickRandom [] = error "List can't be empty"
pickRandom xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i, take i xs ++ drop (i+1) xs)

initGame :: Text -> Options Unwrapped -> Game
initGame defaultConfig os = NotStarted w h m
    where w = fromMaybe dw (O.width os)
          h = fromMaybe dh (O.height os)
          m' = fromMaybe dm (O.mines os)
          m = if m' < w * h then m' else w * h
          (Just dos) = unwrapRecordPure (words defaultConfig)
          (Just dw) = O.width (dos :: Options Unwrapped)
          (Just dh) = O.height (dos :: Options Unwrapped)
          (Just dm) = O.mines (dos :: Options Unwrapped)

setMines :: Game -> Pos -> IO Game
setMines g@Game {} _ = return g
setMines g (x, y) = do
    let w = g ^. width
        h = g ^. height
        m = g ^. minesTotal
        board = M.fromList [((x, y), Closed) | x <- [0..(w - 1)], y <- [0..(h - 1)]]
    ms <- S.fromList . fst <$> foldM (\(ams, ms') _ -> do
                                          (mine, ms'') <- pickRandom ms'
                                          return (mine:ams, ms''))
                      ([], [(x', y') | x' <- [0..(w - 1)], y' <- [0..(h - 1)], x' /= x || y' /= y]) [1..m]
    return (Game w h (C m) m ms board (C 0))


openCell :: Game -> Pos -> Game
openCell g@NotStarted{} _ = g
openCell g pos@(x,y) = case pos `M.lookup` b of
                           Just Closed | pos `S.member` ms -> g & board %~ M.insert pos BOOM
                                       | mc == 0 -> openAround (g & board %~ M.insert pos Empty) pos
                                       | otherwise -> g & board %~ M.insert pos (Around mc)
                           _ -> g
    where mc = minesAround g pos
          b = g ^. board
          ms = g ^. mines

minesAround :: Game -> Pos -> Int
minesAround NotStarted{} _ = error "Game wasn't initialized!"
minesAround g (x, y) =(sum . map (\pos' -> if pos' `S.member` ms then 1 else 0))
                          [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    where ms = g ^. mines

markedAsMineAround :: Game -> Pos -> Int
markedAsMineAround NotStarted{} _ = error "Game wasn't initialized!"
markedAsMineAround g (x, y) =(sum . map (\pos' -> case pos' `M.lookup` b of
                                                      Just Mine -> 1
                                                      _         -> 0))
                                 [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
    where b = g ^. board

invertMine :: Game -> Pos -> Game
invertMine g@NotStarted{} _ = g
invertMine g pos = case pos `M.lookup` (g ^. board) of
                     Just Closed     -> gm
                     Just Mine       -> gc
                     Just Suspicious -> gm
                     _               -> g
    where gm = g & board %~ M.insert pos Mine & minesCount +~ (-1)
          gc = g & board %~ M.insert pos Closed & minesCount +~ 1

invertSuspicious :: Game -> Pos -> Game
invertSuspicious g@NotStarted{} _ = g
invertSuspicious g pos@(x, y) = case pos `M.lookup` (g ^. board) of
                                  Just Closed     -> gs
                                  Just Mine       -> gs
                                  Just Suspicious -> gc
                                  _               -> g
    where gs = g & board %~ M.insert pos Suspicious
          gc = g & board %~ M.insert pos Closed

openAround :: Game -> Pos -> Game
openAround g@NotStarted{} _ = g
openAround g (x, y) = foldl (\g' pos -> let g'' = openCell g' pos in
                                      case pos `M.lookup` (g' ^. board) of
                                          Just Closed -> g''
                                          _           -> g')
                          g [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

discover :: Game -> Pos -> Game
discover g@NotStarted{} _ = g
discover g pos@(x, y) = case pos `M.lookup` (g ^. board) of
                            Just (Around x) | markedAsMineAround g pos == x -> openAround g pos
                            _ -> g

revealMines :: Game -> Game
revealMines g@NotStarted{} = g
revealMines g = g & board %~ M.mapWithKey check
    where check pos cell = case cell of
                               Mine | pos `S.notMember` ms    -> WrongMine
                               Closed | pos `S.member` ms     -> HiddenMine
                               Suspicious | pos `S.member` ms -> HiddenMine
                               _                              -> cell
          ms = g ^. mines

foundMines :: Game -> Int
foundMines g@NotStarted{} = 0
foundMines g = S.size . S.filter (\pos -> case pos `M.lookup` b of
                                              Just Mine -> True
                                              _         -> False) . view mines $ g
    where b = g ^. board

checkResult :: Game -> Result
checkResult NotStarted{} = Playing
checkResult g@Game{}
    | isExploded g = Loser
    | (g ^. minesCount) == 0 && allDefined g = Winner
    | otherwise = Playing

isExploded :: Game -> Bool
isExploded NotStarted{} = False
isExploded g = any (\pos -> b M.!? pos == Just BOOM) . S.toList . view mines $ g
    where b = g ^. board

allDefined :: Game -> Bool
allDefined NotStarted{} = False
allDefined g = all (\case Empty -> True
                          Mine -> True
                          Around _ -> True
                          BOOM -> True
                          _ -> False) . M.elems . view board $ g

tick :: Game -> Game
tick g@NotStarted{} = g
tick g              = g & playTime +~ 1
