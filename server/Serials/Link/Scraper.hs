
-- scraper holds functions that consume tags and produce a value
-- so this isn't quite what I want
-- i want to consume SOME tags, and keep the rest


newtype Scraper' a = MkScraper ([Tag Text] -> (Maybe a, [Tag Text]))

runScrape :: Scraper' a -> [Tag Text] -> (Maybe a, [Tag Text])
runScrape (MkScraper f) = f

instance Functor Scraper' where
    fmap f (MkScraper g) = MkScraper (\tags -> let (x, tags') = g tags
                                                   in (fmap f x, tags'))

instance Applicative Scraper' where
    pure = return
    f <*> x = do
      f' <- f
      x' <- x
      return (f' x')


instance Monad Scraper' where
    return a = MkScraper (\tags -> (Just a, tags))
    (MkScraper a) >>= f = MkScraper combined
      where 
      combined tags 
        | (Just aVal, tags') <- a tags = let (MkScraper b) = f aVal
                                         in  b tags'
        | otherwise                    = (Nothing, tags)

instance Alternative Scraper' where
    empty = MkScraper (\tags -> (Nothing, tags))
    (MkScraper a) <|> (MkScraper b) = MkScraper choice
        where choice tags | (Just aVal, tags') <- a tags = (Just aVal, tags')
                          | otherwise                    = b tags

instance MonadPlus Scraper' where
    mzero = empty
    a `mplus` b = undefined

many' :: Scraper' a -> Scraper' [a]
many' a = MkScraper next
  where
  next []   = (Just [], [])
  next tags = case runScrape a tags of
    (Nothing, ts) -> (Just [], ts)
    (Just v , ts) -> 
      let (Just vs, ts') = next ts
      in (Just (v:vs), ts')


-- finds many a but separated by stuff
-- try the scraper first, see if it works
-- but if it doesn't, then use up one tag and try again
sparse' :: Scraper' a -> Scraper' [a]
sparse' a = MkScraper next
  where
  next []   = (Just [], [])
  next tags = case runScrape a tags of
    (Nothing, ts) -> next (drop 1 ts)
    (Just v , ts) -> 
      let (Just vs, ts') = next ts
      in (Just (v:vs), ts')


  -- run again with new tags

scrapeContents = sparse' scrapeContent

scrapeContent :: Scraper' Content
scrapeContent = scrapeAnchor <|> scrapeTitle

scrapeAnchor :: Scraper' Content
scrapeAnchor = do
    open' "a"
    href <- attr' "href"
    txt  <- forward' text'
    forward' $ close' "a"
    return $ Link href txt

scrapeTitle :: Scraper' Content
scrapeTitle = do
    -- keep scraping text until you hit an "a" tag
    -- I want to get the inner text until a close tag
    -- gather all the text!
    -- it
    txt <- text'
    txt' <- forward' $ many' text'
    --forward' $ open' "a"
    return $ Title (txt <> mconcat txt')

-- just looks at the current tags
get :: Scraper' [Tag Text]
get = MkScraper $ \tags -> (Just tags, tags)

-- marks which tags we've consumed
put :: [Tag Text] -> Scraper' ()
put tags = MkScraper $ const (Just (), tags)

-- consumes any one tag
tag :: Scraper' (Tag Text)
tag = do
    ts <- get
    case ts of
      []     -> mzero
      (t:ts') -> do
        put ts'
        return t


-- looks at the first tag
current ::  Scraper' (Tag Text)
current = do
    ts <- get
    case ts of
      []     -> mzero
      (t:_)  -> return t


-- doesn't consume
open' :: Text -> Scraper' ()
open' name = do
    --traceM $ "WOOOO" <> show name
    t <- current
    --guard $ isTagOpenName name t
    return ()

-- should consume a tag
close' :: Text -> Scraper' ()
close' name = do
    t <- tag
    guard $ isTagCloseName name t

-- doesn't consume
attr' :: Text -> Scraper' Text
attr' name = do
    t <- current
    case maybeAttr name t of
      Nothing -> mzero
      Just v  -> return v

-- fails if the text is empty before the next closing tag
text' :: Scraper' Text
text' = do
    t <- tag
    case maybeTagText t of
      Nothing -> mzero
      Just txt -> case strip txt of
        "" -> mzero
        txt' -> return txt'

 --if we're on a text node, keep grabbing text until... what?
--allText' :: Scraper' Text
--allText'
-- until' 

not' :: Scraper' a -> Scraper' ()
not' a = MkScraper go
  where
  go tags = case runScrape a tags of
              (Nothing, ts) -> (Just (), ts)
              _             -> (Nothing, tags)

forward' :: Scraper' a -> Scraper' a
forward' a = do
    many' $ not' a
    a

maybeAttr :: Text -> Tag Text -> Maybe Text
maybeAttr name (TagOpen _ as) = lookup name as
maybeAttr _ _ = Nothing

-- keep doing b until a
-- if 
--until' :: Scraper' a -> Scraper' b -> Scraper' b
--until' a b = MkScraper go
  --where
  --go tags = case runScrape a tags of
              --(Nothing, _) -> flip runScrape tags do
                --a
    --resB <- b
    ---- resA <- a
    ---- if a is next, then stop otherwise loop
    --return r


