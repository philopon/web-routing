{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Network.Routing.Dict
    ( -- * store
      Store
    , emptyStore
    , type (</)
    , add
    , mkDict

      -- * dictionary
    , Dict
    , emptyDict
    , Member
    , get

      -- * types
    , ShowDict
    , KV(..)

      -- * convenient
    , Members
    ) where

import GHC.Exts(Constraint)
import Network.Routing.Dict.Internal

-- | type family to constraint multi kvs.
--
-- > Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family   Members (kvs :: [KV *]) (prms :: [KV *]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)
