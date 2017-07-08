module Text.Wrap
  ( wrapText
  , tokenize
  )
where

import Data.Char (isSpace)
import qualified Data.Text as T

wrapText :: Int -> T.Text -> [T.Text]
wrapText amt s = concat $ wrapLine amt <$> T.lines s

data Token = WS T.Text | NonWS T.Text
           deriving (Show)

tokenLength :: Token -> Int
tokenLength (WS t) = T.length t
tokenLength (NonWS t) = T.length t

tokenContent :: Token -> T.Text
tokenContent (WS t) = t
tokenContent (NonWS t) = t

tokenize :: T.Text -> [Token]
tokenize t | T.null t = []
tokenize t =
    let leadingWs = T.takeWhile isSpace t
        leadingNonWs = T.takeWhile (not . isSpace) t
        tok = if T.null leadingWs
              then NonWS leadingNonWs
              else WS leadingWs
    in tok : tokenize (T.drop (tokenLength tok) t)

wrapLine :: Int -> T.Text -> [T.Text]
wrapLine limit t =
    let go []     = []
        go [WS _] = []
        go [tok]  = [tokenContent tok]
        go ts =
            let (firstLine, maybeRest) = breakTokens limit ts
                firstLineText = T.stripEnd $ T.concat $ tokenContent <$> firstLine
            in case maybeRest of
                Nothing -> [firstLineText]
                Just rest -> firstLineText : go rest
    in go (tokenize t)

breakTokens :: Int -> [Token] -> ([Token], Maybe [Token])
breakTokens _ [] = ([], Nothing)
breakTokens _ [t] = ([t], Nothing)
breakTokens limit ts =
    -- Take enough tokens until we reach the point where taking more
    -- would exceed the line length.
    let go _ [] = []
        go acc (tok:_) | acc == 0 && tokenLength tok > limit = [tok]
        go acc (tok:toks) =
            if tokenLength tok + acc <= limit
            then tok : go (acc + tokenLength tok) toks
            else []

        allowed = go 0 ts
        rest = maybeTrim $ drop (length allowed) ts

        maybeTrim [] = []
        maybeTrim (WS _:toks) = toks
        maybeTrim toks = toks

        result = if null rest
                 then (allowed, Nothing)
                 else (allowed, Just rest)
    in result
