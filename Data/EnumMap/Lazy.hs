{-# LANGUAGE CPP #-}
-- |
-- Module      :  $Header$
-- Description :  Data.IntMap.Lazy with Enum keys.
-- Copyright   :  (c) 2011-2019 Michal Terepeta
--                (c) 2019-2020 Mikolaj Konarski and others (see git history)
-- License     :  BSD3
-- Maintainer  :  mikolaj.konarski@funktory.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntMap.Lazy' that works with any type of
-- keys that are instances of 'Enum' type class.  For documentation please see
-- the one for 'Data.IntMap'.

module Data.EnumMap.Lazy
  ( EnumMap

  -- * Wrapping/unwrapping
  , intMapToEnumMap
  , enumMapToIntMap

  -- * Operators
  , (!)
  , (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
  , lookup
  , findWithDefault
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE

  -- * Construction
  , empty
  , singleton

  -- ** Insertion
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey

  -- ** Delete\/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter

  -- * Combine

  -- ** Union
  , union
  , unionWith
  , unionWithKey
  , unions
  , unionsWith

  -- ** Difference
  , difference
  , differenceWith
  , differenceWithKey

  -- ** Intersection
  , intersection
  , intersectionWith
  , intersectionWithKey

  -- ** Universal combining function
  , mergeWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , mapKeysMonotonic

  -- * Folds
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  -- ** Strict folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'

  -- * Conversion
  , elems
  , keys
  , assocs
  , keysSet
  , fromSet

  -- ** Lists
  , toList
  , fromList
  , fromListWith
  , fromListWithKey

  -- ** Ordered lists
  , toAscList
  , toDescList
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

  -- * Filter
  , filter
  , filterWithKey
#if (MIN_VERSION_containers(0,5,8))
  , restrictKeys
  , withoutKeys
#endif
  , partition
  , partitionWithKey

  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey

  , split
  , splitLookup

  -- * Submap
  , isSubmapOf
  , isSubmapOfBy
  , isProperSubmapOf
  , isProperSubmapOfBy

  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , updateMin
  , updateMax
  , updateMinWithKey
  , updateMaxWithKey
  , minView
  , maxView
  , minViewWithKey
  , maxViewWithKey
  ) where

import Prelude hiding (filter, foldl, foldr, lookup, map, null)

import Data.EnumMap.Base
