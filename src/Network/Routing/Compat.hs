{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}

-- | compatibility module for ghc-7.8 & ghc-7.6.
module Network.Routing.Compat
    ( Symbol, KnownSymbol, symbolVal
    , Proxy(..)
    ) where

import GHC.TypeLits

#if __GLASGOW_HASKELL__ > 707
import Data.Proxy
#else
type KnownSymbol (n :: Symbol) = SingRep n String

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String
symbolVal _ = fromSing (sing :: Sing n)
{-# INLINE symbolVal #-}

data Proxy (t :: k) = Proxy
#endif
