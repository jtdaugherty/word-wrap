module Text.Wrap
  ( wrapText
  )
where

import qualified Data.Text as T

wrapText :: Int -> T.Text -> [T.Text]
wrapText = const $ const []
