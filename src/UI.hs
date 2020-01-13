{-# LANGUAGE OverloadedStrings #-}

module UI (startApp) where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Maybe                 (fromMaybe)
import           Minesweeper

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (..), EventM, Next,
                                             Padding (..), Widget, attrMap,
                                             continue, customMain, emptyWidget,
                                             fg, hBox, hLimit, halt, locL,
                                             neverShowCursor, on, padBottom,
                                             padLeft, padRight, str, txt, vBox,
                                             vLimit, withAttr, withBorderStyle,
                                             (<+>), (<=>))
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Lens               ((&), (+~), (^.), _1, _2)
import qualified Data.Map                   as M
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Graphics.Vty               as V

data Tick = Tick

type Name = ()

data GameState = GS Pos Game Result

app :: App GameState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

startApp :: Game -> IO ()
startApp g = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000
    let builder = V.mkVty V.defaultConfig
    vty <- builder
    (GS pos g res) <- customMain vty builder (Just chan) app (GS (0, 0) g Playing)
    case res of
        Playing -> putStrLn "Well.. Next time! :)"
        Winner -> putStrLn $ "You won in " ++ show (g ^. playTime) ++ " seconds! :)"
        Loser -> putStrLn $ "BOOM! You lose in " ++ show (g ^. playTime) ++ " seconds. :("

-- Handling events

handleEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleEvent (GS pos g@Game{} Playing) (AppEvent Tick)                 = continue $ GS pos (tick g) Playing
handleEvent (GS pos g res) (AppEvent Tick)                            = continue $ GS pos g res
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey V.KUp []))          = continue . goto $ GS (pos & _2 +~ (-1)) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey V.KDown []))        = continue . goto $ GS (pos & _2 +~ 1) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey V.KRight []))       = continue . goto $ GS (pos & _1 +~ 1) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey V.KLeft []))        = continue . goto $ GS (pos & _1 +~ (-1)) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar 'k') []))  = continue . goto $ GS (pos & _2 +~ (-1)) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar 'j') []))  = continue . goto $ GS (pos & _2 +~ 1) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar 'l') []))  = continue . goto $ GS (pos & _1 +~ 1) g Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar 'h') []))  = continue . goto $ GS (pos & _1 +~ (-1)) g Playing
handleEvent gs@(GS _ g@Game{} Playing) (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue . doAndCheck openCell $ gs
handleEvent gs@(GS pos g@NotStarted{} Playing) (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    g' <- liftIO (setMines g pos)
    continue . doAndCheck openCell $ GS pos g' Playing
handleEvent gs@(GS _ _ Playing) (VtyEvent (V.EvKey (V.KChar 'm') [])) = continue . doAndCheck invertMine $ gs
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar '/') []))  = continue $ GS pos (invertSuspicious g pos) Playing
handleEvent (GS pos g Playing) (VtyEvent (V.EvKey (V.KChar '?') []))  = continue $ GS pos (invertSuspicious g pos) Playing
handleEvent gs@(GS _ _ Playing) (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue . doAndCheck discover $ gs
handleEvent gs@(GS _ _ Playing) (VtyEvent (V.EvKey V.KEnter []))      = continue . doAndCheck discover $ gs
handleEvent (GS pos g res) (VtyEvent (V.EvKey (V.KChar 'q') []))      = halt (GS pos g res)
handleEvent (GS pos g res) (VtyEvent (V.EvKey V.KEsc []))             = halt (GS pos g res)
handleEvent (GS pos g Playing) _                                      = continue (GS pos g Playing)
handleEvent (GS pos g res) _                                          = halt (GS pos g res)

doAndCheck :: (Game -> Pos -> Game) -> GameState -> GameState
doAndCheck action (GS pos g res) = case res of
                                       Playing -> GS pos g' Playing
                                       _       -> GS pos (revealMines g') res
    where g' = action g pos
          res = checkResult g'

goto :: GameState -> GameState
goto (GS (x, y) g res) = GS (x', y') g res
    where w' = (g ^. width) - 1
          h' = (g ^. height) - 1
          x' | x < 0 = 0
             | x > w' = w'
             | otherwise = x
          y' | y < 0 = 0
             | y > h' = h'
             | otherwise = y

drawUI :: GameState -> [Widget Name]
drawUI gs@(GS pos g res) = [ C.hCenter $ hLimit w  $ padBottom (Pad 1) (drawStats g) <=> drawGrid gs <=> drawFinal res ]
    where w = (g ^. width) + 2

drawStats :: Game -> Widget Name
drawStats g = withBorderStyle BS.unicodeBold
            $ B.border
            $ hBox [ padLeft (Pad 1) (str $ show (g ^. minesCount))
                   , str " | "
                   , padRight (Pad 1) (str $ show (g ^. playTime))
                   ]

drawGrid :: GameState -> Widget Name
drawGrid (GS pos g res) = withBorderStyle BS.unicodeBold
                  $ B.border
                  $ vBox rows
    where w = g ^. width
          h = g ^. height
          b = g ^. board
          rows = [hBox $ cellsInRow r | r <- [0..h - 1]]
          cellsInRow y = [drawCoord (x, y) | x <- [0..w - 1]]
          drawCoord = drawCell . cellAt
          cellAt p = case p `M.lookup` b of
                         Just c -> (c, p == pos)
                         _      -> (Closed, p == pos)

drawCell :: (Cell, Bool) -> Widget Name
drawCell (Closed, False)     = withAttr defaultClosedAttr (cw "#")
drawCell (Closed, True)      = withAttr currentClosedAttr (cw "#")
drawCell (Empty, False)      = withAttr defaultEmptyAttr (cw " ")
drawCell (Empty, True)       = withAttr currentEmptyAttr (cw " ")
drawCell (Suspicious, False) = withAttr defaultSuspiciousAttr (cw "?")
drawCell (Suspicious, True)  = withAttr currentSuspiciousAttr (cw "?")
drawCell (Mine, False)       = withAttr defaultMineAttr (cw "+")
drawCell (Mine, True)        = withAttr currentMineAttr (cw "+")
drawCell (Around 1, False)   = withAttr defaultAround1Attr (cw "1")
drawCell (Around 1, True)    = withAttr currentAround1Attr (cw "1")
drawCell (Around 2, False)   = withAttr defaultAround2Attr (cw "2")
drawCell (Around 2, True)    = withAttr currentAround2Attr (cw "2")
drawCell (Around 3, False)   = withAttr defaultAround3Attr (cw "3")
drawCell (Around 3, True)    = withAttr currentAround3Attr (cw "3")
drawCell (Around 4, False)   = withAttr defaultAround4Attr (cw "4")
drawCell (Around 4, True)    = withAttr currentAround4Attr (cw "4")
drawCell (Around 5, False)   = withAttr defaultAround5Attr (cw "5")
drawCell (Around 5, True)    = withAttr currentAround5Attr (cw "5")
drawCell (Around 6, False)   = withAttr defaultAround6Attr (cw "6")
drawCell (Around 6, True)    = withAttr currentAround6Attr (cw "6")
drawCell (Around 7, False)   = withAttr defaultAround7Attr (cw "7")
drawCell (Around 7, True)    = withAttr currentAround7Attr (cw "7")
drawCell (Around _, False)   = withAttr defaultAround8Attr (cw "8")
drawCell (Around _, True)    = withAttr currentAround8Attr (cw "8")
drawCell (BOOM, False)       = withAttr defaultBOOMAttr (cw "X")
drawCell (BOOM, True)        = withAttr currentBOOMAttr (cw "X")
drawCell (WrongMine, False)  = withAttr defaultWrongMineAttr (cw "0")
drawCell (WrongMine, True)   = withAttr currentWrongMineAttr (cw "0")
drawCell (HiddenMine, False) = withAttr defaultHiddenMineAttr (cw "*")
drawCell (HiddenMine, True)  = withAttr currentHiddenMineAttr (cw "*")

cw :: Text -> Widget Name
cw = txt

drawFinal :: Result -> Widget Name
drawFinal Playing = emptyWidget
drawFinal res = withBorderStyle BS.unicodeBold
              $ B.border
              $ C.hCenter
              $ withAttr gameOverAttr
              $ txt msg
    where msg = case res of
                    Winner -> "You Won!"
                    _      -> "You Lose!"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (currentClosedAttr, V.green `on` V.brightBlue)
  , (defaultClosedAttr, V.green `on` V.black)
  , (currentMineAttr, V.brightRed `on` V.brightBlue)
  , (defaultMineAttr, V.brightRed `on` V.black)
  , (currentEmptyAttr, V.brightBlue `on` V.brightBlue)
  , (defaultEmptyAttr, V.black `on` V.black)
  , (currentSuspiciousAttr, V.red `on` V.brightBlue)
  , (defaultSuspiciousAttr, V.red `on` V.black)
  , (currentAround1Attr, V.yellow `on` V.brightBlue)
  , (defaultAround1Attr, V.yellow `on` V.black)
  , (currentAround2Attr, V.brightYellow `on` V.brightBlue)
  , (defaultAround2Attr, V.brightYellow `on` V.black)
  , (currentAround3Attr, V.cyan `on` V.brightBlue)
  , (defaultAround3Attr, V.cyan `on` V.black)
  , (currentAround4Attr, V.brightCyan `on` V.brightBlue)
  , (defaultAround4Attr, V.brightCyan `on` V.black)
  , (currentAround5Attr, V.magenta `on` V.brightBlue)
  , (defaultAround5Attr, V.magenta `on` V.black)
  , (currentAround6Attr, V.brightMagenta `on` V.brightBlue)
  , (defaultAround6Attr, V.brightMagenta `on` V.black)
  , (currentAround7Attr, V.white `on` V.brightBlue)
  , (defaultAround7Attr, V.white `on` V.black)
  , (currentAround8Attr, V.brightWhite `on` V.brightBlue)
  , (defaultAround8Attr, V.brightWhite `on` V.black)
  , (currentBOOMAttr, V.brightRed `on` V.black)
  , (defaultBOOMAttr, V.brightRed `on` V.black)
  , (currentHiddenMineAttr, V.brightRed `on` V.black)
  , (defaultHiddenMineAttr, V.brightRed `on` V.black)
  , (currentWrongMineAttr, V.brightRed `on` V.black)
  , (defaultWrongMineAttr, V.brightRed `on` V.black)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

currentClosedAttr :: AttrName
currentClosedAttr = "currentClosedAttr"
defaultClosedAttr :: AttrName
defaultClosedAttr = "defaultClosedAttr"
currentMineAttr :: AttrName
currentMineAttr = "currentMineAttr"
defaultMineAttr :: AttrName
defaultMineAttr = "defaultMineAttr"
currentEmptyAttr :: AttrName
currentEmptyAttr = "currentEmptyAttr"
defaultEmptyAttr :: AttrName
defaultEmptyAttr = "defaultEmptyAttr"
currentSuspiciousAttr :: AttrName
currentSuspiciousAttr = "currentSuspiciousAttr"
defaultSuspiciousAttr :: AttrName
defaultSuspiciousAttr = "defaultSuspiciousAttr"
currentAround1Attr :: AttrName
currentAround1Attr = "currentAround1Attr"
defaultAround1Attr :: AttrName
defaultAround1Attr = "defaultAround1Attr"
currentAround2Attr :: AttrName
currentAround2Attr = "currentAround2Attr"
defaultAround2Attr :: AttrName
defaultAround2Attr = "defaultAround2Attr"
currentAround3Attr :: AttrName
currentAround3Attr = "currentAround3Attr"
defaultAround3Attr :: AttrName
defaultAround3Attr = "defaultAround3Attr"
currentAround4Attr :: AttrName
currentAround4Attr = "currentAround4Attr"
defaultAround4Attr :: AttrName
defaultAround4Attr = "defaultAround4Attr"
currentAround5Attr :: AttrName
currentAround5Attr = "currentAround5Attr"
defaultAround5Attr :: AttrName
defaultAround5Attr = "defaultAround5Attr"
currentAround6Attr :: AttrName
currentAround6Attr = "currentAround6Attr"
defaultAround6Attr :: AttrName
defaultAround6Attr = "defaultAround6Attr"
currentAround7Attr :: AttrName
currentAround7Attr = "currentAround7Attr"
defaultAround7Attr :: AttrName
defaultAround7Attr = "defaultAround7Attr"
currentAround8Attr :: AttrName
currentAround8Attr = "currentAround8Attr"
defaultAround8Attr :: AttrName
defaultAround8Attr = "defaultAround8Attr"
currentBOOMAttr :: AttrName
currentBOOMAttr = "currentBOOMAttr"
defaultBOOMAttr :: AttrName
defaultBOOMAttr = "defaultBOOMAttr"
currentHiddenMineAttr :: AttrName
currentHiddenMineAttr = "currentHiddenMineAttr"
defaultHiddenMineAttr :: AttrName
defaultHiddenMineAttr = "defaultHiddenMineAttr"
currentWrongMineAttr :: AttrName
currentWrongMineAttr = "currentWrongMineAttr"
defaultWrongMineAttr :: AttrName
defaultWrongMineAttr = "defaultWrongMineAttr"
gameOverAttr :: AttrName
gameOverAttr = "gameOver"
