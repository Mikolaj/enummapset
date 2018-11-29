-- |
-- Module      :  $Header$
-- Description :  Data.IntMap.Strict with Enum keys.
-- Copyright   :  (c) 2011-2013 Michal Terepeta
-- License     :  BSD3
-- Maintainer  :  michal.terepeta@gmail.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntMap.Strict' that works with any type of
-- keys that are instances of 'Enum' type class.  For documentation please see
-- the one for 'Data.IntMap'.

module Data.EnumMap.Strict
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
  , restrictKeys
  , withoutKeys
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

import Prelude hiding ( filter, foldr, foldl, lookup, map, null )
import qualified Prelude as P

import Control.Arrow ( (***), first, second )

import qualified Data.IntMap.Strict as I

import Data.EnumSet ( EnumSet )
import qualified Data.EnumSet as EnumSet

import Data.EnumMap.Base hiding
  ( findWithDefault
  , singleton
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , unionsWith
  , unionWith
  , unionWithKey
  , differenceWith
  , differenceWithKey
  , intersectionWith
  , intersectionWithKey
  , mergeWithKey
  , updateMinWithKey
  , updateMaxWithKey
  , updateMax
  , updateMin
  , map
  , mapWithKey
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey
  , mapKeysWith
  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey
  , fromSet
  , fromList
  , fromListWith
  , fromListWithKey
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList
  )

findWithDefault :: (Enum k) => a -> k -> EnumMap k a -> a
findWithDefault def k = I.findWithDefault def (fromEnum k) . unWrap
{-# INLINE findWithDefault #-}

singleton :: (Enum k) => k -> a -> EnumMap k a
singleton k = EnumMap . I.singleton (fromEnum k)
{-# INLINE singleton #-}

insert :: (Enum k) => k -> a -> EnumMap k a -> EnumMap k a
insert k a = EnumMap . I.insert (fromEnum k) a . unWrap
{-# INLINE insert #-}

insertWith :: (Enum k) => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith f k a = EnumMap . I.insertWith f (fromEnum k) a . unWrap
{-# INLINE insertWith #-}

insertWithKey :: (Enum k)
  => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k a =
  EnumMap . I.insertWithKey (f . toEnum) (fromEnum k) a . unWrap
{-# INLINE insertWithKey #-}

insertLookupWithKey :: (Enum k)
  => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k a =
  second EnumMap . I.insertLookupWithKey (f . toEnum) (fromEnum k) a . unWrap
{-# INLINE insertLookupWithKey #-}

adjust ::  (Enum k) => (a -> a) -> k -> EnumMap k a -> EnumMap k a
adjust f k = EnumMap . I.adjust f (fromEnum k) . unWrap
{-# INLINE adjust #-}

adjustWithKey :: (Enum k) => (k -> a -> a) -> k -> EnumMap k a -> EnumMap k a
adjustWithKey f k = EnumMap . I.adjustWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE adjustWithKey #-}

alter :: (Enum k) => (Maybe a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
alter f k = EnumMap . I.alter f (fromEnum k) . unWrap
{-# INLINE alter #-}

unionsWith :: (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
unionsWith f = EnumMap . I.unionsWith f . P.map unWrap
{-# INLINE unionsWith #-}

unionWith :: (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f (EnumMap im1) (EnumMap im2) = EnumMap $ I.unionWith f im1 im2
{-# INLINE unionWith #-}

unionWithKey :: (Enum k)
  => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.unionWithKey (f . toEnum) im1 im2
{-# INLINE unionWithKey #-}

differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.differenceWith f im1 im2
{-# INLINE differenceWith #-}

differenceWithKey :: (Enum k)
  => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.differenceWithKey (f . toEnum) im1 im2
{-# INLINE differenceWithKey #-}

intersectionWith :: (a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWith f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.intersectionWith f im1 im2
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Enum k)
  => (k -> a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWithKey f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.intersectionWithKey (f . toEnum) im1 im2
{-# INLINE intersectionWithKey #-}

mergeWithKey :: (Enum k)
  => (k -> a -> b -> Maybe c)
  -> (EnumMap k a -> EnumMap k c)
  -> (EnumMap k b -> EnumMap k c)
  -> EnumMap k a
  -> EnumMap k b
  -> EnumMap k c
mergeWithKey f ga gb = \ma mb -> EnumMap $
  I.mergeWithKey (f . toEnum)
                 (unWrap . ga . EnumMap)
                 (unWrap . gb . EnumMap)
                 (unWrap ma)
                 (unWrap mb)
{-# INLINE mergeWithKey #-}

update ::  (Enum k) => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k = EnumMap . I.update f (fromEnum k) . unWrap
{-# INLINE update #-}

updateWithKey ::  (Enum k)
  => (k -> a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
updateWithKey f k = EnumMap . I.updateWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE updateWithKey #-}

updateLookupWithKey ::  (Enum k)
  => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a,EnumMap k a)
updateLookupWithKey f k =
  second EnumMap . I.updateLookupWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE updateLookupWithKey #-}

updateMinWithKey :: (Enum k) => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMinWithKey f = EnumMap . I.updateMinWithKey (f . toEnum) . unWrap
{-# INLINE updateMinWithKey #-}

updateMaxWithKey :: (Enum k) => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey f = EnumMap . I.updateMaxWithKey (f . toEnum) . unWrap
{-# INLINE updateMaxWithKey #-}

updateMax :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMax f = EnumMap . I.updateMax f . unWrap
{-# INLINE updateMax #-}

updateMin :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMin f = EnumMap . I.updateMin f . unWrap
{-# INLINE updateMin #-}

map :: (a -> b) -> EnumMap k a -> EnumMap k b
map f = EnumMap . I.map f . unWrap
{-# INLINE map #-}

mapWithKey :: (Enum k) => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f = EnumMap . I.mapWithKey (f . toEnum) . unWrap
{-# INLINE mapWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccum f a = second EnumMap . I.mapAccum f a . unWrap
{-# INLINE mapAccum #-}

mapAccumWithKey :: (Enum k)
  => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey f a =
  second EnumMap . I.mapAccumWithKey (\b -> f b . toEnum) a . unWrap
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: (Enum k)
  => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumRWithKey f a =
  second EnumMap . I.mapAccumRWithKey (\b -> f b . toEnum) a . unWrap
{-# INLINE mapAccumRWithKey #-}

mapKeysWith :: (Enum k) => (a -> a -> a) -> (k -> k) -> EnumMap k a -> EnumMap k a
mapKeysWith f g = EnumMap . I.mapKeysWith f (fromEnum . g . toEnum) . unWrap
{-# INLINE mapKeysWith #-}

mapMaybe :: (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe f = EnumMap . I.mapMaybe f . unWrap
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Enum k) => (k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey f = EnumMap . I.mapMaybeWithKey (f . toEnum) . unWrap
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither f = (EnumMap *** EnumMap) . I.mapEither f . unWrap
{-# INLINE mapEither #-}

mapEitherWithKey :: (Enum k)
  => (k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey f =
  (EnumMap *** EnumMap) . I.mapEitherWithKey (f . toEnum) . unWrap
{-# INLINE mapEitherWithKey #-}

fromSet :: (Enum k) => (k -> a) -> EnumSet k -> EnumMap k a
fromSet f = EnumMap . I.fromSet (f . toEnum) . EnumSet.enumSetToIntSet
{-# INLINE fromSet #-}

fromList :: (Enum k) => [(k, a)] -> EnumMap k a
fromList = EnumMap . I.fromList . P.map (first fromEnum)
{-# INLINE fromList #-}

fromListWith :: (Enum k) => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWith f = EnumMap . I.fromListWith f . P.map (first fromEnum)
{-# INLINE fromListWith #-}

fromListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWithKey f =
  EnumMap . I.fromListWithKey (f . toEnum) . P.map (first fromEnum)
{-# INLINE fromListWithKey #-}

fromAscList :: (Enum k) => [(k, a)] -> EnumMap k a
fromAscList = EnumMap . I.fromAscList . P.map (first fromEnum)
{-# INLINE fromAscList #-}

fromAscListWith :: (Enum k) => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWith f = EnumMap . I.fromAscListWith f . P.map (first fromEnum)
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWithKey f =
  EnumMap . I.fromAscListWithKey (f . toEnum) . P.map (first fromEnum)
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: (Enum k) => [(k, a)] -> EnumMap k a
fromDistinctAscList = EnumMap . I.fromDistinctAscList . P.map (first fromEnum)
{-# INLINE fromDistinctAscList #-}
