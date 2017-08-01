module Text.Wrap
  ( wrapTextToLines
  , wrapText
  )
where

import Data.Char (isSpace)
import qualified Data.Text as T

-- | Wrap text at the specified width. Newlines and whitespace in the
-- input text are preserved. Returns the lines of text in wrapped form.
-- New lines introduced due to wrapping will have leading whitespace
-- stripped.
wrapTextToLines :: Int -> T.Text -> [T.Text]
wrapTextToLines amt s = concat $ fmap (wrapLine amt) $ T.lines s

-- | Like 'wrapTextToLines', but returns the wrapped text reconstructed
-- with newlines inserted at wrap points.
wrapText :: Int -> T.Text -> T.Text
wrapText amt s = T.intercalate (T.pack "\n") $ wrapTextToLines amt s

data Token = WS T.Text | NonWS T.Text
           deriving (Show)

tokenLength :: Token -> Int
tokenLength (WS t) = T.length t
tokenLength (NonWS t) = T.length t

tokenContent :: Token -> T.Text
tokenContent (WS t) = t
tokenContent (NonWS t) = t

-- | Tokenize text into whitespace and non-whitespace chunks.
tokenize :: T.Text -> [Token]
tokenize t | T.null t = []
tokenize t =
    let leadingWs = T.takeWhile isSpace t
        leadingNonWs = T.takeWhile (not . isSpace) t
        tok = if T.null leadingWs
              then NonWS leadingNonWs
              else WS leadingWs
    in tok : tokenize (T.drop (tokenLength tok) t)

-- | Wrap a single line of text into a list of lines that all satisfy
-- the wrapping width.
wrapLine :: Int
         -- ^ The wrapping width.
         -> T.Text
         -- ^ A single line of text.
         -> [T.Text]
wrapLine limit t =
    let go []     = [T.empty]
        go [WS _] = []
        go [tok]  = [tokenContent tok]
        go ts =
            let (firstLine, maybeRest) = breakTokens limit ts
                firstLineText = T.stripEnd $ T.concat $ fmap tokenContent firstLine
            in case maybeRest of
                Nothing -> [firstLineText]
                Just rest -> firstLineText : go rest
    in go (tokenize t)

-- | Break a token sequence so that all tokens up to but not exceeding
-- a length limit are included on the left, and if any remain on the
-- right, return Just those too (or Nothing if there weren't any). If
-- this breaks a sequence at at point where the next token after the
-- break point is whitespace, that whitespace token is removed.
breakTokens :: Int -> [Token] -> ([Token], Maybe [Token])
breakTokens _ [] = ([], Nothing)
breakTokens _ [t] = ([t], Nothing)
breakTokens limit ts =
    -- Take enough tokens until we reach the point where taking more
    -- would exceed the line length.
    let go _ [] = []
        -- If the line starts with a token that itself exceeds the
        -- limit, just take that single token as the only one on this
        -- line.
        go acc (tok:_) | acc == 0 && tokenLength tok > limit = [tok]
        -- Otherwise take a token if its length plus the accumulator
        -- doesn't exceed the limit.
        go acc (tok:toks) =
            if tokenLength tok + acc <= limit
            then tok : go (acc + tokenLength tok) toks
            else []

        -- Allowed tokens are the ones we keep on this line.
        allowed = go 0 ts
        -- The rest go on the next line, to be wrapped again.
        rest = maybeTrim $ drop (length allowed) ts

        -- Trim leading whitespace on wrapped lines.
        maybeTrim [] = []
        maybeTrim (WS _:toks) = toks
        maybeTrim toks = toks

        result = if null rest
                 then (allowed, Nothing)
                 else (allowed, Just rest)
    in result
