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
import Debug.Trace
import Data.Maybe (isJust, isNothing)
import Control.Monad (foldM)
import Data.Functor ((<&>))


newtype FileContents = FileContents String deriving (Show)
newtype Tokenized = Tokenized [String] deriving (Show)
newtype Parsed = Parsed [String] deriving (Show)
-- | GADT describing queries the compiler can respond to.
data Query a where -- a is the return value of the query
    QFileRead :: String -> Query FileContents
    QTokenizeFile :: Query FileContents -> Query Tokenized
    QParseFile :: Query Tokenized -> Query Parsed
deriving instance Eq (Query a)
deriving instance Ord (Query a)
deriving instance Show (Query a)
deriving instance Typeable (Query a)
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
    fetch :: (Typeable a, Show a) => Query a -> m a
    store :: Query a -> a -> m ()
    invalidate :: Query a -> m ()
    isStale :: Query a -> m Bool

newtype QueryCache = QueryCache { unQueryCache :: D.DMap Query Identity }

instance (MonadIO m, MonadState QueryCache m) => Cache m where
    store :: MonadState QueryCache m => Query a -> a -> m ()
    store query payload = modify (QueryCache . D.insert query (pure payload) . unQueryCache)
    invalidate :: MonadState QueryCache m => Query a -> m ()
    invalidate query = modify (QueryCache . D.delete query . unQueryCache)
    isStale :: MonadState QueryCache m => Query a -> m Bool
    isStale query = get >>= (pure . isNothing . D.lookup query . unQueryCache)
    fetch :: (Typeable a, Show a, MonadState QueryCache m) => Query a -> m a
    fetch query = do
        liftIO $ traceIO ("Fetching " ++ show query)
        let deps = depends query
            deps' = pure <$> deps :: [m (Some Query)]
            depsStale = deps' <&> \dep -> withSomeM dep isStale
        needsUpdate <- sequence depsStale
        let stale = or needsUpdate
        liftIO $ traceIO (" ... with deps " ++ show deps)
        liftIO $ traceIO (" ... stale?    " ++ show needsUpdate ++ if stale then " => Yup" else " => Nope")
        if stale then do
            payload <- route query 
            store query payload
            traceM ("Cache outcome: stale dep! " ++ show (toDyn payload) ++ " " ++ show payload)
            pure payload
        else do
            cache <- unQueryCache <$> get
            case D.lookup query cache of
                Just (Identity payload) -> do
                    traceM ("Cache outcome: hit! " ++ show (toDyn payload) ++ " " ++ show payload)
                    pure payload
                Nothing -> do
                    payload <- route query 
                    store query payload
                    traceM ("Cache outcome: miss! " ++ show (toDyn payload) ++ " " ++ show payload)
                    pure payload

concrete :: StateT QueryCache IO ()
concrete = do
    liftIO $ putStrLn "\n#### RUN 1 #### NO CACHE ####"
    let fileq = QFileRead "hello-world.txt"
    _ <- fetch (QParseFile . QTokenizeFile $ fileq)
    liftIO $ putStrLn "\n#### RUN 2 #### FULLY CACHED ####"
    _ <- fetch (QParseFile . QTokenizeFile $ fileq)
    liftIO $ putStrLn "\n#### RUN 3 #### INVALIDATED INNER CACHE ####"
    invalidate fileq
    _ <- fetch (QParseFile . QTokenizeFile $ fileq)
    pure ()

main :: IO ()
main = evalStateT concrete (QueryCache mempty)
