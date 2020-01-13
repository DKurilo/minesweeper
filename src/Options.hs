{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Options
    ( Options(..)
    , defaultConfig
    ) where

import           Data.Text       (Text)
import           Options.Generic

data Options w = Options { width  :: w ::: Maybe Int <?> "Board width"
                         , height :: w ::: Maybe Int <?> "Board height"
                         , mines  :: w ::: Maybe Int <?> "Mines amount"
                         } deriving (Generic)

instance ParseRecord (Options Wrapped)

defaultConfig :: Text
defaultConfig = "--width 10 --height 10 --mines 10"
