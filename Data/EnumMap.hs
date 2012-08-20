{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  $Header$
-- Description :  Data.IntMap with Enum keys.
-- Copyright   :  (c) 2011 Michal Terepeta
-- License     :  BSD3
-- Maintainer  :  michal.terepeta@gmail.com
-- Stability   :  alpha
-- Portability :  uses GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntMap' that works with any type of keys
-- that are instances of 'Enum' type class. Useful if one wants to have the
-- performance of 'Data.IntMap' and at the same time use something else than
-- 'Int's (e.g. an 'Int' wrapped with newtype). For documentation please see the
-- one for 'Data.IntMap'.

module Data.EnumMap
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
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey

  -- ** Fold
  , fold
  , foldWithKey

  -- * Conversion
  , elems
  , keys
  , keysSet
  , assocs

  -- ** Lists
  , toList
  , fromList
  , fromListWith
  , fromListWithKey

  -- ** Ordered lists
  , toAscList
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

  -- * Filter
  , filter
  , filterWithKey
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

import Prelude hiding ( filter, lookup, map, null )
import qualified Prelude as P

import Data.IntMap ( IntMap )
import qualified Data.IntMap as I

import Data.EnumSet ( EnumSet )
import qualified Data.EnumSet as EnumSet

import Control.Arrow ( first, second, (***) )
import Data.Foldable ( Foldable )
import Data.Monoid ( Monoid )
import Data.Traversable ( Traversable )
import Data.Typeable ( Typeable )

import Text.Read

-- | Wrapper for 'IntMap' with 'Enum' keys.
newtype EnumMap k a = EnumMap { unWrap :: IntMap a }
  deriving (Eq, Foldable, Functor, Ord, Monoid, Traversable, Typeable)

instance (Enum k, Show k, Show a) => Show (EnumMap k a) where
  showsPrec p em = showParen (p > 10) $
    showString "fromList " . shows (toList em)

instance (Enum k, Read k, Read a) => Read (EnumMap k a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    list <- readPrec
    return (fromList list)

--
-- Conversion to/from 'IntMap'.
--

-- | Wrap 'IntMap'.
intMapToEnumMap :: IntMap a -> EnumMap k a
intMapToEnumMap = EnumMap

-- | Unwrap 'IntMap'.
enumMapToIntMap :: EnumMap k a -> IntMap a
enumMapToIntMap = unWrap

--
-- A few useful functions used through the module. Not exported.
--

pairWrap :: (IntMap a, IntMap b) -> (EnumMap k a, EnumMap k b)
pairWrap = EnumMap *** EnumMap
{-# INLINE pairWrap #-}

--
-- Here begins the main part.
--

(!) :: (Enum k) => EnumMap k a -> k -> a
(EnumMap im) ! k = im I.! (fromEnum k)
{-# INLINE (!) #-}

(\\) :: EnumMap k a -> EnumMap k b -> EnumMap k a
(EnumMap im1) \\ (EnumMap im2) = EnumMap $ im1 I.\\ im2
{-# INLINE (\\) #-}

null :: EnumMap k a -> Bool
null = I.null . unWrap
{-# INLINE null #-}

size :: EnumMap k a -> Int
size = I.size . unWrap
{-# INLINE size #-}

member :: (Enum k) => k -> EnumMap k a -> Bool
member k = I.member (fromEnum k) . unWrap
{-# INLINE member #-}

notMember :: (Enum k) => k -> EnumMap k a -> Bool
notMember k = I.notMember (fromEnum k) . unWrap
{-# INLINE notMember #-}

lookup :: (Enum k) => k -> EnumMap k a -> Maybe a
lookup k = I.lookup (fromEnum k) . unWrap
{-# INLINE lookup #-}

findWithDefault :: (Enum k) => a -> k -> EnumMap k a -> a
findWithDefault def k = I.findWithDefault def (fromEnum k) . unWrap
{-# INLINE findWithDefault #-}

empty :: EnumMap k a
empty = EnumMap $ I.empty
{-# INLINE empty #-}

singleton :: (Enum k) => k -> a -> EnumMap k a
singleton k = EnumMap . I.singleton (fromEnum k)
{-# INLINE singleton #-}

insert :: (Enum k) => k -> a -> EnumMap k a -> EnumMap k a
insert k a = EnumMap . I.insert (fromEnum k) a . unWrap
{-# INLINE insert #-}

insertWith :: (Enum k) => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith f k a = EnumMap . I.insertWith f (fromEnum k) a . unWrap
{-# INLINE insertWith #-}

insertWithKey :: (Enum k) => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k a = EnumMap . I.insertWithKey (f . toEnum) (fromEnum k) a . unWrap
{-# INLINE insertWithKey #-}

insertLookupWithKey :: (Enum k) => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k a = second EnumMap . I.insertLookupWithKey (f . toEnum) (fromEnum k) a . unWrap
{-# INLINE insertLookupWithKey #-}

delete :: (Enum k) => k -> EnumMap k a -> EnumMap k a
delete k = EnumMap . I.delete (fromEnum k) . unWrap
{-# INLINE delete #-}

adjust ::  (Enum k) => (a -> a) -> k -> EnumMap k a -> EnumMap k a
adjust f k = EnumMap . I.adjust f (fromEnum k) . unWrap
{-# INLINE adjust #-}

adjustWithKey :: (Enum k) => (k -> a -> a) -> k -> EnumMap k a -> EnumMap k a
adjustWithKey f k = EnumMap . I.adjustWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE adjustWithKey #-}

update ::  (Enum k) => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k = EnumMap . I.update f (fromEnum k) . unWrap
{-# INLINE update #-}

updateWithKey ::  (Enum k) => (k -> a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
updateWithKey f k = EnumMap . I.updateWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE updateWithKey #-}

updateLookupWithKey ::  (Enum k) => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a,EnumMap k a)
updateLookupWithKey f k = second EnumMap . I.updateLookupWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE updateLookupWithKey #-}

alter :: (Enum k) => (Maybe a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
alter f k = EnumMap . I.alter f (fromEnum k) . unWrap
{-# INLINE alter #-}

unions :: [EnumMap k a] -> EnumMap k a
unions = EnumMap . I.unions . P.map unWrap
{-# INLINE unions #-}

unionsWith :: (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
unionsWith f = EnumMap . I.unionsWith f . P.map unWrap
{-# INLINE unionsWith #-}

union :: EnumMap k a -> EnumMap k a -> EnumMap k a
union (EnumMap im1) (EnumMap im2) = EnumMap $ I.union im1 im2
{-# INLINE union #-}

unionWith :: (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f (EnumMap im1) (EnumMap im2) = EnumMap $ I.unionWith f im1 im2
{-# INLINE unionWith #-}

unionWithKey :: (Enum k) => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f (EnumMap im1) (EnumMap im2) = EnumMap $ I.unionWithKey (f . toEnum) im1 im2
{-# INLINE unionWithKey #-}

difference :: EnumMap k a -> EnumMap k b -> EnumMap k a
difference (EnumMap im1) (EnumMap im2) = EnumMap $ I.difference im1 im2
{-# INLINE difference #-}

differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f (EnumMap im1) (EnumMap im2) = EnumMap $ I.differenceWith f im1 im2
{-# INLINE differenceWith #-}

differenceWithKey :: (Enum k) => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f (EnumMap im1) (EnumMap im2) = EnumMap $ I.differenceWithKey (f . toEnum) im1 im2
{-# INLINE differenceWithKey #-}

intersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
intersection (EnumMap im1) (EnumMap im2) = EnumMap $ I.intersection im1 im2
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWith f (EnumMap im1) (EnumMap im2) = EnumMap $ I.intersectionWith f im1 im2
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Enum k) => (k -> a -> b -> c) -> EnumMap k a -> EnumMap k b -> EnumMap k c
intersectionWithKey f (EnumMap im1) (EnumMap im2) = EnumMap $ I.intersectionWithKey (f . toEnum) im1 im2
{-# INLINE intersectionWithKey #-}

mergeWithKey :: (Enum k) => (k -> a -> b -> Maybe c) ->
    (EnumMap k a -> EnumMap k c) -> (EnumMap k b -> EnumMap k c) ->
    EnumMap k a -> EnumMap k b -> EnumMap k c
mergeWithKey f ga gb = \ ma mb -> EnumMap $
    I.mergeWithKey (f . toEnum) (unWrap . ga . EnumMap) (unWrap . gb . EnumMap) (unWrap ma) (unWrap mb)
{-# INLINE mergeWithKey #-}

updateMinWithKey :: (Enum k) => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMinWithKey f = EnumMap . I.updateMinWithKey (f . toEnum) . unWrap
{-# INLINE updateMinWithKey #-}

updateMaxWithKey :: (Enum k) => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey f = EnumMap . I.updateMaxWithKey (f . toEnum) . unWrap
{-# INLINE updateMaxWithKey #-}

maxViewWithKey :: (Enum k) => EnumMap k a -> Maybe ((k, a), EnumMap k a)
maxViewWithKey = fmap wrap . I.maxViewWithKey . unWrap
  where
    wrap ((i, a), im) = ((toEnum i, a), EnumMap im)
{-# INLINE maxViewWithKey #-}

minViewWithKey :: (Enum k) => EnumMap k a -> Maybe ((k, a), EnumMap k a)
minViewWithKey =  fmap wrap . I.minViewWithKey . unWrap
  where
    wrap ((i, a), imap) = ((toEnum i, a), EnumMap imap)
{-# INLINE minViewWithKey #-}

updateMax :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMax f = EnumMap . I.updateMax f . unWrap
{-# INLINE updateMax #-}

updateMin :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMin f = EnumMap . I.updateMin f . unWrap
{-# INLINE updateMin #-}

maxView :: EnumMap k a -> Maybe (a, EnumMap k a)
maxView = fmap (second EnumMap) . I.maxView . unWrap
{-# INLINE maxView #-}

minView :: EnumMap k a -> Maybe (a, EnumMap k a)
minView = fmap (second EnumMap) . I.minView . unWrap
{-# INLINE minView #-}

deleteFindMax :: (Enum k) => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMax = (first toEnum *** EnumMap) . I.deleteFindMax . unWrap
{-# INLINE deleteFindMax #-}

deleteFindMin :: (Enum k) => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMin = (first toEnum *** EnumMap) . I.deleteFindMin . unWrap
{-# INLINE deleteFindMin #-}

findMin :: (Enum k) => EnumMap k a -> (k, a)
findMin = first toEnum . I.findMin . unWrap
{-# INLINE findMin #-}

findMax :: (Enum k) => EnumMap k a -> (k, a)
findMax = first toEnum . I.findMax . unWrap
{-# INLINE findMax #-}

deleteMin :: EnumMap k a -> EnumMap k a
deleteMin = EnumMap . I.deleteMin . unWrap
{-# INLINE deleteMin #-}

deleteMax :: EnumMap k a -> EnumMap k a
deleteMax = EnumMap . I.deleteMax . unWrap
{-# INLINE deleteMax #-}

isProperSubmapOf :: (Eq a) => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOf (EnumMap im1) (EnumMap im2) = I.isProperSubmapOf im1 im2
{-# INLINE isProperSubmapOf #-}

isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOfBy p (EnumMap im1) (EnumMap im2) = I.isProperSubmapOfBy p im1 im2
{-# INLINE isProperSubmapOfBy #-}

isSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isSubmapOf (EnumMap im1) (EnumMap im2) = I.isSubmapOf im1 im2
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isSubmapOfBy p (EnumMap im1) (EnumMap im2) = I.isSubmapOfBy p im1 im2
{-# INLINE isSubmapOfBy #-}

map :: (a -> b) -> EnumMap k a -> EnumMap k b
map f = EnumMap . I.map f . unWrap
{-# INLINE map #-}

mapWithKey :: (Enum k) => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f = EnumMap . I.mapWithKey (f . toEnum) . unWrap
{-# INLINE mapWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccum f a = second EnumMap . I.mapAccum f a . unWrap
{-# INLINE mapAccum #-}

mapAccumWithKey :: (Enum k) => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey f a = second EnumMap . I.mapAccumWithKey (\b -> f b . toEnum) a . unWrap
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: (Enum k) => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumRWithKey f a = second EnumMap . I.mapAccumRWithKey (\b -> f b . toEnum) a . unWrap
{-# INLINE mapAccumRWithKey #-}

filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filter p = EnumMap . I.filter p . unWrap
{-# INLINE filter #-}

filterWithKey :: (Enum k) => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey p = EnumMap . I.filterWithKey (p . toEnum) . unWrap
{-# INLINE filterWithKey #-}

partition :: (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partition p = pairWrap . I.partition p . unWrap
{-# INLINE partition #-}

partitionWithKey :: (Enum k) => (k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey p = pairWrap . I.partitionWithKey (p . toEnum) . unWrap
{-# INLINE partitionWithKey #-}

mapMaybe :: (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe f = EnumMap . I.mapMaybe f . unWrap
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Enum k) => (k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey f = EnumMap . I.mapMaybeWithKey (f . toEnum) . unWrap
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither f = pairWrap . I.mapEither f . unWrap
{-# INLINE mapEither #-}

mapEitherWithKey :: (Enum k) => (k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey f = pairWrap . I.mapEitherWithKey (f . toEnum) . unWrap
{-# INLINE mapEitherWithKey #-}

split :: (Enum k) => k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split k = pairWrap . I.split (fromEnum k) . unWrap
{-# INLINE split #-}

splitLookup :: (Enum k) => k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup k = wrap . I.splitLookup (fromEnum k) . unWrap
  where
    wrap (im1, ma, im2) = (EnumMap im1, ma, EnumMap im2)
{-# INLINE splitLookup #-}

fold :: (a -> b -> b) -> b -> EnumMap k a -> b
fold f acc = I.fold f acc . unWrap
{-# INLINE fold #-}

foldWithKey :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey f acc = I.foldWithKey (f . toEnum) acc . unWrap
{-# INLINE foldWithKey #-}

elems :: EnumMap k a -> [a]
elems = I.elems . unWrap
{-# INLINE elems #-}

keys :: (Enum k) => EnumMap k a -> [k]
keys = P.map toEnum . I.keys . unWrap
{-# INLINE keys #-}

keysSet :: (Enum k) => EnumMap k a -> EnumSet k
keysSet = EnumSet.fromDistinctAscList . keys
{-# INLINE keysSet #-}

assocs :: (Enum k) => EnumMap k a -> [(k, a)]
assocs = P.map (first toEnum) . I.assocs . unWrap
{-# INLINE assocs #-}

toList :: (Enum k) => EnumMap k a -> [(k, a)]
toList = P.map (first toEnum) . I.toList . unWrap
{-# INLINE toList #-}

toAscList :: (Enum k) => EnumMap k a -> [(k, a)]
toAscList = P.map (first toEnum) . I.toAscList . unWrap
{-# INLINE toAscList #-}

fromList :: (Enum k) => [(k, a)] -> EnumMap k a
fromList = EnumMap . I.fromList . P.map (first fromEnum)
{-# INLINE fromList #-}

fromListWith :: (Enum k) => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWith f = EnumMap . I.fromListWith f . P.map (first fromEnum)
{-# INLINE fromListWith #-}

fromListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWithKey f = EnumMap . I.fromListWithKey (f . toEnum) . P.map (first fromEnum)
{-# INLINE fromListWithKey #-}

fromAscList :: (Enum k) => [(k, a)] -> EnumMap k a
fromAscList = EnumMap . I.fromAscList . P.map (first fromEnum)
{-# INLINE fromAscList #-}

fromAscListWith :: (Enum k) => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWith f = EnumMap . I.fromAscListWith f . P.map (first fromEnum)
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWithKey f = EnumMap . I.fromAscListWithKey (f . toEnum) . P.map (first fromEnum)
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: (Enum k) => [(k, a)] -> EnumMap k a
fromDistinctAscList = EnumMap . I.fromDistinctAscList . P.map (first fromEnum)
{-# INLINE fromDistinctAscList #-}
