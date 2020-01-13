{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Minesweeper
import           Options         (defaultConfig)
import           Options.Generic (Unwrapped, unwrapRecord)
import           UI

main :: IO ()
main = do
    config <- unwrapRecord ("Minesweeper" <> "\27EDefault options: " <> defaultConfig)
    let game = initGame defaultConfig config
    startApp game
