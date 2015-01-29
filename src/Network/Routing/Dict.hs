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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Network.Routing.Dict
    ( -- * dictionary
      Dict
    , ShowDict
    , KV(..)
    , empty

      -- * insert
    , type (</)
    , add

      -- * get
    , Member
    , get

      -- * convenient
    , Members
    ) where

import GHC.Exts(Constraint)

import Network.Routing.Tree

#if __GLASGOW_HASKELL__ > 707
import Data.Proxy
import GHC.TypeLits
#endif

import Data.Typeable(typeOf, Typeable, TypeRep)
import Data.List(intercalate)
import Unsafe.Coerce

-- | (kind) key-value pair
data KV v = Symbol := v

-- | heterogeneous dictionary
newtype Dict (kvs :: [KV *]) = Dict Tree

class ShowDict (kvs :: [KV *]) where
    showDict :: Int -> Dict kvs -> [(String, String, TypeRep)]

instance ShowDict '[] where
    showDict _ _ = []

instance (KnownSymbol k, Typeable v, Show v, ShowDict kvs) => ShowDict (k := v ': kvs) where
    showDict i (Dict t) =
        (symbolVal (Proxy :: Proxy k), show (unsafeCoerce $ index t i :: v), typeOf (undefined :: v)):
        showDict (i + 1) (unsafeCoerce $ Dict t :: Dict kvs)

instance ShowDict kvs => Show (Dict kvs) where
    show d = "Dict {" ++
        (intercalate ", " . map (\(k, v, t) -> k ++ " = " ++ v ++ " :: " ++ show t) $ showDict 0 d)
        ++ "}"

empty :: Dict '[]
empty = Dict Tip

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

-- | add key value pair to dictionary.
--
-- >>> let a = add (Proxy :: Proxy "foo") (12 :: Int) empty
-- >>> a
-- Dict {foo = 12 :: Int}
--
-- >>> let b = add (Proxy :: Proxy "bar") "baz" a
-- >>> b
-- Dict {bar = "baz" :: [Char], foo = 12 :: Int}
add :: (k </ kvs) => proxy k -> v -> Dict kvs -> Dict (k := v ': kvs)
add _ v (Dict d) = Dict (unsafeCoerce v `cons` d)

#if __GLASGOW_HASKELL__ > 707
type family Ix (k :: Symbol) (kvs :: [KV *]) :: Nat where
  Ix k (k  := v ': kvs) = 0
  Ix k (k' := v ': kvs) = 1 + Ix k kvs

getImpl :: forall proxy k kvs v. KnownNat (Ix k kvs) => proxy (k :: Symbol) -> Dict kvs -> v
getImpl _ (Dict d) = unsafeCoerce $ d `index` fromIntegral (natVal (Proxy :: Proxy (Ix k kvs)))

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
    get' !i _ (Dict d) = unsafeCoerce $ d `index` i

instance Member k v kvs => Member k v (k' := v' ': kvs) where
    get' !i k d = get' (i + 1) k (unsafeCoerce d :: Dict kvs)

get = get' 0
#endif

-- | get key from dictionary
--
-- >>> let d = add (Proxy :: Proxy "foo") 12 $ add (Proxy :: Proxy "bar") "baz" empty
-- >>> get (Proxy :: Proxy "foo") d
-- 12
-- >>> get (Proxy :: Proxy "bar") d
-- "baz"
get :: Member k v kvs => proxy k -> Dict kvs -> v

-- | type family to constraint multi kvs.
--
-- > Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family   Members (kvs :: [KV *]) (prms :: [KV *]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)
