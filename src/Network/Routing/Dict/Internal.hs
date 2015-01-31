{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-} -- for ghc-7.6
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Network.Routing.Dict.Internal
    ( Dict, Store
    , ShowDict
    , KV(..)
    , empty
    , type (</)
    , add
    , Member
    , get
    , mkDict
    ) where

import GHC.Exts(Any)

import GHC.TypeLits
import Network.Routing.Compat
import Data.Typeable(typeOf, Typeable, TypeRep)
import Data.List(intercalate)
import Unsafe.Coerce
import qualified Control.Monad.Primitive as P
import qualified Data.Primitive as P
import Control.Monad.ST (ST, runST)

-- | (kind) key-value pair
data KV v = Symbol := v

-- | data store to construct dictionary.
--
-- `add` and `mkDict` operation only allowed.
data Store (kvs :: [KV *]) where
    Cons  :: {-# UNPACK #-} !Int -> v -> Store kvs -> Store (k := v ': kvs)
    Empty :: Store '[]

instance ShowDict kvs => Show (Store kvs) where
    show d = "Store {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 (mkDict d))
        ++ "}"

empty :: Store '[]
empty = Empty

size :: Store kvs -> Int
size Empty        = 0
size (Cons l _ _) = l

-- | result type for pretty printing type error.
data HasKeyResult
    = AlreadyExists Symbol
    | Dictionary

#if __GLASGOW_HASKELL__ > 707
type family HasKey (k :: Symbol) (kvs :: [KV *]) :: HasKeyResult where
  HasKey k '[] = AlreadyExists k
  HasKey k (k  := v ': kvs) = Dictionary
  HasKey k (k' := v ': kvs) = HasKey k kvs
#else
type family   HasKey (k :: Symbol) (kvs :: [KV *]) :: HasKeyResult
type instance HasKey k kvs = AlreadyExists k
#endif

-- | 'not elem key' constraint(ghc >= 7.8)
type k </ v = HasKey k v ~ AlreadyExists k

-- | O(1) add key value pair to dictionary.
--
-- >>> let a = add (Proxy :: Proxy "foo") (12 :: Int) empty
-- >>> a
-- Store {foo = 12 :: Int}
--
-- >>> add (Proxy :: Proxy "bar") "baz" a
-- Store {bar = "baz" :: [Char], foo = 12 :: Int}
add :: (k </ kvs) => proxy k -> v -> Store kvs -> Store (k := v ': kvs)
add _ v Empty          = Cons 1 v Empty
add _ v c@(Cons i _ _) = Cons (i + 1) v c

-- | heterogeneous dictionary
--
-- 'get' operation only allowed.
newtype Dict (kvs :: [KV *]) = Dict (P.Array Any)

class ShowDict (kvs :: [KV *]) where
    showDict :: Int -> Dict kvs -> [(String, String, TypeRep)]

instance ShowDict '[] where
    showDict _ _ = []

instance (KnownSymbol k, Typeable v, Show v, ShowDict kvs) => ShowDict (k := v ': kvs) where
    showDict i (Dict t) =
        (symbolVal (Proxy :: Proxy k), show (unsafeCoerce $ P.indexArray t i :: v), typeOf (undefined :: v)):
        showDict (i + 1) (unsafeCoerce $ Dict t :: Dict kvs)

instance ShowDict kvs => Show (Dict kvs) where
    show d = "Dict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 d)
        ++ "}"

mkDict' :: forall s kvs. Store kvs -> ST s (Dict kvs)
mkDict' store = do
    ary <- P.newArray (size store) undefined
    go (size store) ary
    Dict `fmap` P.unsafeFreezeArray ary
  where
    go :: Int -> P.MutableArray (P.PrimState (ST s)) Any -> ST s ()
    go size' array = loop store
      where
        loop :: Store kvs -> ST s ()
        loop (Cons i v ss) = do
            P.writeArray array (size' - i) (unsafeCoerce v)
            loop (unsafeCoerce ss)
        loop Empty = return ()

-- | O(n) convert "Store" to "Dictionary".
mkDict :: Store kvs -> Dict kvs
mkDict store = runST $ mkDict' store

-- | O(1) (>= ghc-7.8), O(n) (< ghc-7.8) get key from dictionary
--
-- >>> let d = mkDict $ add (Proxy :: Proxy "foo") 12 $ add (Proxy :: Proxy "bar") "baz" empty
-- >>> get (Proxy :: Proxy "foo") d
-- 12
-- >>> get (Proxy :: Proxy "bar") d
-- "baz"
get :: Member k v kvs => proxy k -> Dict kvs -> v

#if __GLASGOW_HASKELL__ < 707
type family Ix (k :: Symbol) (kvs :: [KV *]) :: Nat where
  Ix k (k  := v ': kvs) = 0
  Ix k (k' := v ': kvs) = 1 + Ix k kvs

getImpl :: forall proxy k kvs v. KnownNat (Ix k kvs) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeCoerce $ d `P.indexArray` fromIntegral (natVal (Proxy :: Proxy (Ix k kvs)))

class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get' = getImpl
    {-# INLINE get' #-}

instance (Member k v kvs, KnownNat (Ix k (k' := v' ': kvs))) => Member k v (k' := v' ': kvs) where
    get' = getImpl
    {-# INLINE get' #-}

get = get'
#else
class Member (k :: Symbol) (v :: *) (kvs :: [KV *]) | k kvs -> v where
    get' :: Int -> proxy k -> Dict kvs -> v

instance Member k v (k := v ': kvs) where
    get' !i _ (Dict d) = unsafeCoerce $ d `P.indexArray` i

instance Member k v kvs => Member k v (k' := v' ': kvs) where
    get' !i k d = get' (i + 1) k (unsafeCoerce d :: Dict kvs)

get = get' 0
#endif
