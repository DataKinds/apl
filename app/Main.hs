{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main where
import qualified Data.Dependent.Map as D
import Data.Functor.Identity (Identity (..))
import Control.Monad.State.Lazy 
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Some
import Data.Coerce (coerce)
import Data.Dynamic 


newtype FileContents = FileContents String
newtype Tokenized = Tokenized [String]
newtype Parsed = Parsed [String]
-- | GADT describing queries the compiler can respond to.
data Query a where -- a is the return value of the query
    QFileRead :: String -> Query FileContents
    QTokenizeFile :: Query FileContents -> Query Tokenized
    QParseFile :: Query Tokenized -> Query Parsed
deriving instance Eq (Query a)
deriving instance Ord (Query a)
deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

directDepends :: Query a -> [Some Query]
directDepends = \case
    QFileRead _ -> []
    QTokenizeFile q1 -> [mkSome q1]
    QParseFile q1 -> [mkSome q1]

directDepends' :: Some Query -> [Some Query]
directDepends' = flip withSome directDepends

depends' :: Some Query -> [Some Query]
-- depends q = concat 
--     [   directDepends' q
--     ,   concatMap directDepends' (directDepends' q)
--     ,   concatMap directDepends' (concatMap directDepends' (directDepends' q))
--     ,   ...
--     ]
depends' q = let 
    initial = directDepends' q
    f = concatMap directDepends'
    in concat . takeWhile (not . null) . iterate f $ initial 
    
depends :: Query a -> [Some Query]
depends = depends' . mkSome

-- | Actually carry out a query described by the GADT above
route :: Cache m => Query a -> m a 
route = \case
    QFileRead filepath -> liftIO $ do
        putStrLn ("!! DOING IO TO GRAB FILE " ++ filepath ++ " !!")
        contents <- readFile filepath
        pure $ FileContents contents
    QTokenizeFile fileq -> do
        fileContents <- fetch fileq
        pure $ Tokenized []
    QParseFile tokenq -> do
        tokens <- fetch tokenq
        pure $ Parsed []

-- | Interface and implementation for an object which caches query requests
class MonadIO m => Cache m where
    fetch :: Query a -> m a
    store :: Query a -> a -> m ()
    invalidate :: Query a -> m ()

newtype QueryCache = QueryCache { unQueryCache :: D.DMap Query Identity }

instance (MonadIO m, MonadState QueryCache m) => Cache m where
    store :: MonadState QueryCache m => Query a -> a -> m ()
    store query payload = modify (QueryCache . D.insert query (pure payload) . unQueryCache)
    invalidate :: MonadState QueryCache m => Query a -> m ()
    invalidate query = modify (QueryCache . D.delete query . unQueryCache)
    fetch :: MonadState QueryCache m => Query a -> m a
    fetch query = do
        c <- unQueryCache <$> get
        case D.lookup query c of
            Just x -> pure . runIdentity $ x
            Nothing -> do
                payload <- route query 
                store query payload
                pure payload

concrete :: StateT QueryCache IO ()
concrete = do
    str1 <- fetch (QFileRead "hello-world.txt")
    str2 <- fetch (QFileRead "hello-world.txt")
    str3 <- fetch (QFileRead "hello-world.txt")
    str4 <- fetch (QTokenizeFile $ QFileRead "hello-world.txt")
    str5 <- fetch (QParseFile . QTokenizeFile $ QFileRead "hello-world.txt")
    liftIO . putStrLn $ "Got the following files:\n" ++ (unlines $ show <$> [toDyn str1, toDyn str2, toDyn str3, toDyn str4, toDyn str5])

main :: IO ()
main = evalStateT concrete (QueryCache mempty)
