{-# LANGUAGE TupleSections #-}

module Data.X.Parser
  ( Parser (runParser),
    runParser',
    Result,
    (|>),
    (<|),
    (>>|),
    (|<<),
    satisfy,
    nSatisfy,
    try,

    -- * Basic building blocks
    one,
    Data.X.Parser.max,
    Data.X.Parser.min,
    count,

    -- * Count based combinators
    seperatedBy,
    seperatedBy1,
    forever,
  )
where

import Control.Applicative (Alternative (..), liftA2)

--import Debug.Trace (trace)

type Result s a = (Either String a, [s])

-- | Wrapper for parsing computation
newtype Parser s a = Parser
  { -- | run parsing computation
    runParser :: [s] -> Result s a
  }

runParser' :: Parser s a -> [s] -> (Either (String, Int) a, [s])
runParser' parser stream = case runParser parser stream of
  (Right x, stream') -> (Right x, stream')
  (Left msg, stream') -> (Left (msg, index stream stream'), stream')
  where
    -- index of failure is consumed elements + 1
    index a b = 1 + length a - length b

mapResult :: (a -> b) -> Result s a -> Result s b
mapResult f (Right x, rest) = (Right $ f x, rest)
mapResult _ (Left err, rest) = (Left err, rest)

instance Semigroup a => Semigroup (Parser s a) where
  Parser r1 <> Parser r2 = Parser merge
    where
      merge stream = case r1 stream of
        (Left err, stream') -> (Left err, stream')
        (Right x, stream') -> case r2 stream' of
          (Left err, stream'') -> (Left err, stream'')
          (Right y, stream'') -> (Right $ x <> y, stream'')

instance Semigroup a => Monoid (Parser s a) where
  mempty = Parser (Left "Empty stream",)

instance Functor (Parser s) where
  fmap f (Parser runner) = Parser $ mapResult f . runner

instance Applicative (Parser s) where
  pure a = Parser (Right a,)

  Parser p1 <*> Parser p2 = Parser $
    \stream -> case p1 stream of
      (Left err, stream') -> (Left err, stream')
      (Right f, stream') -> case p2 stream' of
        (Left err, stream'') -> (Left err, stream'')
        (Right x, stream'') -> (Right $ f x, stream'')

instance Alternative (Parser s) where
  empty = Parser (Left "Empty Stream",)

  (Parser r1) <|> (Parser r2) = Parser trySecond
    where
      trySecond stream = case r1 stream of
        (Left _, stream') -> r2 stream'
        (Right x, stream') -> (Right x, stream')

-- | Does next item satisfy the predicate?
satisfy :: String -> (a -> Bool) -> Parser a a
satisfy name predicate = Parser isTrue
  where
    isTrue [] = (Left $ "No items for predicate: " ++ name, [])
    isTrue (p : ps)
      | predicate p = (Right p, ps)
      | otherwise = (Left $ "Predicate failed: " ++ name, ps)

-- | Multi argument version of satisfy
nSatisfy :: Int -> ([a] -> Bool) -> Parser a [a]
nSatisfy n predicate = Parser isAllTrue
  where
    nPredicate = predicate . take n
    isAllTrue [] = (Left "No items for predicate", [])
    isAllTrue stream
      | nPredicate stream = (Right $ take n stream, drop n stream)
      | otherwise = (Left "Predicate failed", stream)

-- | Try a parse and if it fails, reset the @stream@
try :: Parser s a -> Parser s a
try (Parser r) = Parser go
  where
    go stream = case r stream of
      (Left err, _) -> (Left err, stream)
      (Right x, stream') -> (Right x, stream')

-- | A variant of @'<|>'@ that ignores its first argument's results.
(|>) :: Parser s b -> Parser s a -> Parser s a
(Parser sep) |> (Parser p) = Parser go
  where
    go stream = case sep stream of
      (Left _, stream') -> p stream'
      (Right _, stream') -> p stream'

-- | A variant of @'<|>'@ that ignores its second argument's results.
(<|) :: Parser s a -> Parser s b -> Parser s a
(Parser p) <| (Parser sep) = Parser go
  where
    go stream = case sep stream of
      (Left _, stream') -> p stream'
      (Right _, stream') -> p stream'

-- | @p1 >>| p2@ prepends the result of @p1@ to @p2@
(>>|) :: Parser s a -> Parser s [a] -> Parser s [a]
single >>| multi = liftA2 (:) single multi

-- | @p1 >>| p2@ prepends the result of @p1@ to @p2@
(|<<) :: Parser s [a] -> Parser s a -> Parser s [a]
multi |<< single = liftA2 (++) multi (fmap (: []) single)

-- | @one p@ tries a parser until it consumes the entire stream.
-- NOTE: Never use this with @try@, else you are guaranteed an infinite
-- loop
one :: Parser s a -> Parser s a
one (Parser r) = Parser go
  where
    go [] = (Left "End of stream", [])
    go stream = case r stream of
      (Left _, stream') -> go stream'
      (Right x, stream') -> (Right x, stream')

-- | @forever p@ runs the parser till it consumes the entire stream
-- regardless of failure. Like @one@ never use this with @try@
forever :: Parser s a -> Parser s [a]
forever (Parser r) = Parser go
  where
    go [] = (Right [], [])
    go stream = case r stream of
      (Left _, stream') -> go stream'
      (Right x, stream') -> mapResult (x :) $ go stream'

-- | @seperatedBy sep p@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@
seperatedBy :: Parser s b -> Parser s a -> Parser s [a]
seperatedBy sep p = seperatedBy1 sep p <|> pure []

-- | @seperatedBy1 sep p@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@
seperatedBy1 :: Parser s b -> Parser s a -> Parser s [a]
seperatedBy1 sep p = liftA2 (:) p $ many (sep *> p)

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.
count :: Int -> Parser s a -> Parser s [a]
count n p
  | n <= 0 = pure []
  | otherwise = sequenceA (replicate n p)

-- | Parse at least n times
min :: Int -> Parser s a -> Parser s [a]
min n = count n <> many

-- TODO: strictMax

-- | Parse 0-n times. Note that this still succeds even if there
-- are more tokens that pass the parser|:)|.
max :: Int -> Parser s a -> Parser s [a]
max n (Parser r) = Parser $ go n
  where
    go nx stream
      | nx == 0 = (Right [], stream)
      | otherwise = case r stream of
        (Left _, _) -> (Right [], stream)
        (Right x, stream') -> mapResult (x :) $ go (nx - 1) stream'
