-- Note that all these extensions are only needed for @Whoops@ & Co
-- and only as long as "Utils.Containers.Internal.TypeError"
-- is not exposed in the containers package
-- (see https://github.com/haskell/containers/issues/586).
{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, TypeFamilies, UndecidableInstances #-}
-- |
-- Module      :  $Header$
-- Description :  Data.IntMap with Enum keys.
-- Copyright   :  (c) 2011-2019 Michal Terepeta
--                (c) 2019-2020 Mikolaj Konarski and others (see git history)
-- License     :  BSD3
-- Maintainer  :  mikolaj.konarski@funktory.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntMap' that works with any type of keys
-- that are instances of 'Enum' type class.  For documentation please see the
-- one for 'Data.IntMap'.

module Data.EnumMap
  ( module Data.EnumMap.Lazy
#ifdef __GLASGOW_HASKELL__
-- For GHC, we disable these, pending removal. For anything else,
-- we just don't define them at all.
  , insertWith'
  , insertWithKey'
  , fold
  , foldWithKey
#endif
  ) where

import Data.EnumMap.Lazy

#ifdef __GLASGOW_HASKELL__
-- Unfortunately we can't access this module:
-- import Utils.Containers.Internal.TypeError
-- so we copy-paste things first:

import GHC.TypeLits

class Whoops (a :: Symbol)

#if __GLASGOW_HASKELL__ >= 800
instance TypeError ('Text a) => Whoops a
#endif


-- | This function is being removed and is no longer usable.
-- Use 'Data.EnumMap.Strict.insertWith'
insertWith' :: Whoops "Data.EnumMap.insertWith' is gone. Use Data.EnumMap.Strict.insertWith."
            => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.EnumMap.Strict.insertWithKey'.
insertWithKey' :: Whoops "Data.EnumMap.insertWithKey' is gone. Use Data.EnumMap.Strict.insertWithKey."
               => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey' _ _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'Data.EnumMap.Lazy.foldr'.
fold :: Whoops "Data.EnumMap.fold' is gone. Use Data.EnumMap.foldr or Prelude.foldr."
     => (a -> b -> b) -> b -> EnumMap k a -> b
fold _ _ _ = undefined

-- | This function is being removed and is no longer usable.
-- Use 'foldrWithKey'.
foldWithKey :: Whoops "Data.EnumMap.foldWithKey is gone. Use foldrWithKey."
            => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey _ _ _ = undefined
#endif
