module Parser (BF (..), parseBF') where

data BF
  = GoLeft
  | GoRight
  | Inc
  | Dec
  | Write
  | Read
  | Loop [BF]
  deriving (Show, Eq)

parseBF' :: String -> Maybe [BF]
parseBF' s = fst <$> parseBF s 0

parseBF :: String -> Int -> Maybe ([BF], String)
parseBF "" 0 = Just ([], "")
parseBF "" _ = Nothing
parseBF (x : xs) d = case x of
  '>' -> do
    (contents, xs') <- parseBF xs d
    pure (GoRight : contents, xs')
  '<' -> do
    (contents, xs') <- parseBF xs d
    pure (GoLeft : contents, xs')
  '+' -> do
    (contents, xs') <- parseBF xs d
    pure (Inc : contents, xs')
  '-' -> do
    (contents, xs') <- parseBF xs d
    pure (Dec : contents, xs')
  ',' -> do
    (contents, xs') <- parseBF xs d
    pure (Write : contents, xs')
  '.' -> do
    (contents, xs') <- parseBF xs d
    pure (Read : contents, xs')
  '[' -> do
    (contents, xs') <- parseBF xs (d + 1)
    (rest, xs'') <- parseBF xs' d
    pure (Loop contents : rest, xs'')
  ']' -> if d == 0 then Nothing else Just ([], xs)
  _ -> parseBF xs d
