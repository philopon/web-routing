{-# LANGUAGE FlexibleContexts #-} -- for ghc-7.6
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | >>> :set -XDataKinds -XOverloadedStrings
-- >>> import Data.Proxy (Proxy(..))
-- >>> import Text.Read(readMaybe)
-- >>> import qualified Data.Text as T
--
-- 1. create path
--
-- >>> data Result = A | B T.Text | C | D Int deriving Show
-- >>> let a = root $ exact "foo" $ action Nothing (\_ -> Just A)
-- >>> let b = root $ exact "bar" $ fetch (Proxy :: Proxy "key") Just $ action Nothing (\d -> Just . B $ D.get (Proxy :: Proxy "key") d)
-- >>> let c = root $ exact "bar" $ any $ action (Just "GET") (\_ -> Just C)
-- >>> let d = root $ exact "bar" $ fetch (Proxy :: Proxy "key") (\t -> readMaybe (T.unpack t) :: Maybe Int) $ action Nothing (\d -> Just . D $ D.get (Proxy :: Proxy "key") d)
-- >>> a
-- * /foo
-- >>> b
-- * /bar/:key
-- >>> c
-- GET /bar/**
-- >>> d
-- * /bar/:key
--
-- 2. create router
--
-- >>> let r = d +| a +| b +| c +| empty
--
-- 3. execute router
--
-- >>> let run = execute r
--
-- >>> run "GET" ["foo"]
-- Just A
-- >>> run "GET" ["foo", "bar"]
-- Nothing
-- >>> run "GET" ["bar", "12"]
-- Just (D 12)
-- >>> run "GET" ["bar", "baz"]
-- Just (B "baz")
-- >>> run "GET" ["bar", "baz", "qux"]
-- Just C
-- >>> run "POST" ["bar", "baz", "qux"]
-- Nothing

module Network.Routing
    ( Method
      -- * Path 
    , Path
      -- ** Path constructors
    , root
      -- *** children
    , exact
      -- *** action
    , action
      -- *** get parameter
    , raw
    , fetch
    , any
    , rest

    -- * Router
    , Router
    , empty
    , add, (+|)

    -- * execute
    , execute
    ) where

import Prelude hiding(any)
import Control.Monad(MonadPlus(..))

import Network.Routing.Compat(KnownSymbol, symbolVal)
import qualified Network.Routing.Dict as D
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

type Method = S.ByteString

data Params d m a where
    PCons :: (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
          -> Router d' m a
          -> Params d m a -> Params d m a
    PNil  :: Params d m a

-- | routing path
data Path d m a where
    Exact :: T.Text -> Path d m a -> Path d m a

    Param :: String -> (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
          -> Path d' m a -> Path d m a

    Action :: Maybe Method
           -> (D.Dict d -> m a) -> Path d m a

-- | root
--
-- @
-- root == id
-- @
root :: Path '[] m a -> Path '[] m a
root = id

-- | exact matching path
exact :: T.Text -> Path d m a -> Path d m a
exact = Exact

raw :: String -- ^ pretty print
    -> (D.Dict d -> [T.Text] -> m (D.Dict d', [T.Text]))
    -> Path d' m a -> Path d m a
raw = Param

fetch :: (MonadPlus m, KnownSymbol k, k D.</ d)
      => proxy k -> (T.Text -> Maybe v)
      -> Path (k D.:= v ': d) m a -> Path d m a
fetch p f = Param (':' : symbolVal p) go
  where
    go _ [] = mzero
    go d (t:ts) = case f t of
        Nothing -> mzero
        Just v  -> return (D.add p v d, ts)

any :: Monad m => Path d m a -> Path d m a
any = Param "**" go
  where
    go d _ = return (d, [])

rest :: (KnownSymbol k, Monad m, k D.</ d) => proxy k
     -> Path (k D.:= [T.Text] ': d) m a -> Path d m a
rest k = Param (':': symbolVal k ++ "**") go
  where
    go d r = return (D.add k r d, [])

-- | action
action :: Maybe Method -- ^ if Nothing, any method allowed
       -> (D.Dict d -> m a) -- ^ action when route matching
       -> Path d m a
action  = Action

instance Show (Path d m a) where
    show = go id
      where
        go :: (String -> String) -> Path d m a -> String
        go s (Exact t ps) = go (s . (++) ('/' : T.unpack t)) ps
        go s (Param l _ ps) = go (s . (++) ('/': l)) ps
        go s (Action m _) = maybe "*" SC.unpack m ++ ' ': s []

-- | router
data Router d m a where
    Router ::
        { params    :: Params d m a
        , children  :: H.HashMap T.Text (Router d m a)
        , methods   :: H.HashMap Method (D.Dict d -> m a)
        , anyMethod :: D.Dict d -> m a
        } -> Router d m a


emptyRouter :: MonadPlus m => Router d m a
emptyRouter = Router { params    = PNil
                     , children  = H.empty
                     , methods   = H.empty
                     , anyMethod = const mzero
                     }

-- | empty router
empty :: MonadPlus m => Router '[] m a
empty = emptyRouter

add' :: MonadPlus m => Path d m a -> Router d m a -> Router d m a
add' (Exact p n) r =
    let c = H.lookupDefault emptyRouter p (children r)
    in r { children = H.insert p (add' n c) (children r) }

add' (Param _ f n) Router{..} = Router
    { params    = PCons f (add' n emptyRouter) params
    , children  = children
    , methods   = methods
    , anyMethod = anyMethod
    }

add' (Action (Just m) n) r = 
    let c = case H.lookup m (methods r) of
            Nothing -> \d -> n d
            Just p  -> \d -> p d `mplus` n d
    in r { methods = H.insert m c (methods r) }

add' (Action Nothing n) r =
    r { anyMethod = \d -> anyMethod r d `mplus` n d }

-- | insert path to router
add :: MonadPlus m => Path '[] m a -> Router '[] m a -> Router '[] m a
add = add'

(+|) :: MonadPlus m => Path '[] m a -> Router '[] m a -> Router '[] m a
(+|) = add

infixr `add`
infixr +|

-- | execute router
execute :: MonadPlus m => Router '[] m a -> Method -> [T.Text] -> m a
execute = execute' D.empty

execute' :: MonadPlus m => D.Dict d -> Router d m a -> Method -> [T.Text] -> m a
execute' d Router{params, methods, anyMethod} m [] = fetching d m [] params `mplus`
    case H.lookup m methods of
        Nothing -> anyMethod d
        Just f  -> f d

execute' d Router{params, children} m pps@(p:ps) = child `mplus` fetching d m pps params
  where
    child = case H.lookup p children of
        Nothing -> mzero
        Just c  -> execute' d c m ps

fetching :: MonadPlus m => D.Dict d -> Method -> [T.Text] -> Params d m a -> m a
fetching d m pps = loop
  where
    loop PNil = mzero
    loop (PCons f r o) =
        do (d', pps') <- f d pps
           execute' d' r m pps'
        `mplus` loop o
