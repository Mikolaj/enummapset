{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}

-- |
-- Module      :  $Header$
-- Description :  Data.IntMap with Enum keys.
-- Copyright   :  (c) 2011-2019 Michal Terepeta
--                (c) 2019-2022 Mikolaj Konarski and others (see git history)
-- License     :  BSD3
-- Maintainer  :  mikolaj.konarski@funktory.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

module Data.EnumMap.Base
  ( EnumMap(..)

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
  , alterF

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

import Prelude hiding ( filter, foldr, foldl, lookup, map, null )
import qualified Prelude as P

import Control.Applicative ( Applicative, liftA )

import Data.IntMap.Lazy ( IntMap )
import qualified Data.IntMap.Lazy as I

import Data.EnumSet ( EnumSet )
import qualified Data.EnumSet as EnumSet

import Control.Arrow ( first, second, (***) )
import Control.DeepSeq ( NFData )
import Data.Foldable ( Foldable )
import Data.Monoid ( Monoid )
import Data.Semigroup ( Semigroup )
import Data.Traversable ( Traversable )
import Data.Typeable ( Typeable )
import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Text.Read

-- | Wrapper for 'IntMap' with 'Enum' keys.
newtype EnumMap k a = EnumMap { unWrap :: IntMap a }
  deriving (Eq, Foldable, Functor, Ord, Semigroup, Monoid,
            Traversable, Typeable, NFData)

instance (Enum k, Show k, Show a) => Show (EnumMap k a) where
  showsPrec p em = showParen (p > 10) $
    showString "fromList " . shows (toList em)

instance (Enum k, Read k, Read a) => Read (EnumMap k a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    fromList <$> readPrec

instance (ToJSON a) => ToJSON (EnumMap k a) where
    toJSON = toJSON . unWrap
    toEncoding = toEncoding . unWrap

instance (FromJSON a) => FromJSON (EnumMap k a) where
    parseJSON = fmap (EnumMap . I.fromList) . parseJSON

--
-- Conversion to/from 'IntMap'.
--

-- | Wrap 'IntMap'.
intMapToEnumMap :: IntMap a -> EnumMap k a
intMapToEnumMap = EnumMap
{-# INLINE intMapToEnumMap #-}

-- | Unwrap 'IntMap'.
enumMapToIntMap :: EnumMap k a -> IntMap a
enumMapToIntMap = unWrap
{-# INLINE enumMapToIntMap #-}

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

lookupLT :: (Enum k) => k -> EnumMap k a -> Maybe (k, a)
lookupLT k = fmap (first toEnum) . I.lookupLT (fromEnum k) . unWrap
{-# INLINE lookupLT #-}

lookupGT :: (Enum k) => k -> EnumMap k a -> Maybe (k, a)
lookupGT k = fmap (first toEnum) . I.lookupGT (fromEnum k) . unWrap
{-# INLINE lookupGT #-}

lookupLE :: (Enum k) => k -> EnumMap k a -> Maybe (k, a)
lookupLE k = fmap (first toEnum) . I.lookupLE (fromEnum k) . unWrap
{-# INLINE lookupLE #-}

lookupGE :: (Enum k) => k -> EnumMap k a -> Maybe (k, a)
lookupGE k = fmap (first toEnum) . I.lookupGE (fromEnum k) . unWrap
{-# INLINE lookupGE #-}

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

insertWithKey :: (Enum k)
  => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k a = EnumMap . I.insertWithKey (f . toEnum) (fromEnum k) a . unWrap
{-# INLINE insertWithKey #-}

insertLookupWithKey :: (Enum k)
  => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k a =
  second EnumMap . I.insertLookupWithKey (f . toEnum) (fromEnum k) a . unWrap
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

updateLookupWithKey ::  (Enum k)
  => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a,EnumMap k a)
updateLookupWithKey f k =
  second EnumMap . I.updateLookupWithKey (f . toEnum) (fromEnum k) . unWrap
{-# INLINE updateLookupWithKey #-}

alter :: (Enum k) => (Maybe a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
alter f k = EnumMap . I.alter f (fromEnum k) . unWrap
{-# INLINE alter #-}

alterF :: (Enum k, Functor f) => (Maybe a -> f (Maybe a)) -> k -> EnumMap k a -> f (EnumMap k a)
alterF f k = fmap EnumMap . I.alterF f (fromEnum k) . unWrap
{-# INLINE alterF #-}

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

unionWithKey :: (Enum k)
  => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.unionWithKey (f . toEnum) im1 im2
{-# INLINE unionWithKey #-}

difference :: EnumMap k a -> EnumMap k b -> EnumMap k a
difference (EnumMap im1) (EnumMap im2) = EnumMap $ I.difference im1 im2
{-# INLINE difference #-}

differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.differenceWith f im1 im2
{-# INLINE differenceWith #-}

differenceWithKey :: (Enum k)
  => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f (EnumMap im1) (EnumMap im2) =
  EnumMap $ I.differenceWithKey (f . toEnum) im1 im2
{-# INLINE differenceWithKey #-}

intersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
intersection (EnumMap im1) (EnumMap im2) = EnumMap $ I.intersection im1 im2
{-# INLINE intersection #-}

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

traverseWithKey :: (Applicative t, Enum k)
  => (k -> a -> t b) -> EnumMap k a -> t (EnumMap k b)
traverseWithKey f = liftA EnumMap . I.traverseWithKey (f . toEnum) . unWrap
{-# INLINE traverseWithKey #-}

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

mapKeys :: (Enum k) => (k -> k) -> EnumMap k a -> EnumMap k a
mapKeys f = EnumMap . I.mapKeys (fromEnum . f . toEnum) . unWrap
{-# INLINE mapKeys #-}

mapKeysWith :: (Enum k) => (a -> a -> a) -> (k -> k) -> EnumMap k a -> EnumMap k a
mapKeysWith f g = EnumMap . I.mapKeysWith f (fromEnum . g . toEnum) . unWrap
{-# INLINE mapKeysWith #-}

mapKeysMonotonic :: (Enum k) => (k -> k) -> EnumMap k a -> EnumMap k a
mapKeysMonotonic f = EnumMap . I.mapKeysMonotonic (fromEnum . f . toEnum) . unWrap
{-# INLINE mapKeysMonotonic #-}

filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filter p = EnumMap . I.filter p . unWrap
{-# INLINE filter #-}

filterWithKey :: (Enum k) => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey p = EnumMap . I.filterWithKey (p . toEnum) . unWrap
{-# INLINE filterWithKey #-}

#if (MIN_VERSION_containers(0,5,8))
restrictKeys :: (Enum k) => EnumMap k a -> EnumSet k -> EnumMap k a
restrictKeys m s =
  EnumMap $ I.restrictKeys (unWrap m) (EnumSet.enumSetToIntSet s)
{-# INLINE restrictKeys #-}

withoutKeys :: (Enum k) => EnumMap k a -> EnumSet k -> EnumMap k a
withoutKeys m s =
  EnumMap $ I.withoutKeys (unWrap m) (EnumSet.enumSetToIntSet s)
{-# INLINE withoutKeys #-}
#endif

partition :: (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partition p = (EnumMap *** EnumMap) . I.partition p . unWrap
{-# INLINE partition #-}

partitionWithKey :: (Enum k)
   => (k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey p =
  (EnumMap *** EnumMap) . I.partitionWithKey (p . toEnum) . unWrap
{-# INLINE partitionWithKey #-}

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

split :: (Enum k) => k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split k = (EnumMap *** EnumMap) . I.split (fromEnum k) . unWrap
{-# INLINE split #-}

splitLookup :: (Enum k) => k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup k = wrap . I.splitLookup (fromEnum k) . unWrap
  where
    wrap (im1, ma, im2) = (EnumMap im1, ma, EnumMap im2)
{-# INLINE splitLookup #-}

foldr :: (a -> b -> b) -> b -> EnumMap k a -> b
foldr f a = I.foldr f a . unWrap
{-# INLINE foldr #-}

foldl :: (a -> b -> a) -> a -> EnumMap k b -> a
foldl f a = I.foldl f a . unWrap
{-# INLINE foldl #-}

foldrWithKey :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey f a = I.foldrWithKey (f . toEnum) a . unWrap
{-# INLINE foldrWithKey #-}

foldlWithKey :: (Enum k) => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey f a = I.foldlWithKey (\a' -> f a' . toEnum) a . unWrap
{-# INLINE foldlWithKey #-}

foldr' :: (a -> b -> b) -> b -> EnumMap k a -> b
foldr' f a = I.foldr' f a . unWrap
{-# INLINE foldr' #-}

foldl' :: (a -> b -> a) -> a -> EnumMap k b -> a
foldl' f a = I.foldl' f a . unWrap
{-# INLINE foldl' #-}

foldrWithKey' :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey' f a = I.foldrWithKey' (f . toEnum) a . unWrap
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: (Enum k) => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey' f a = I.foldlWithKey' (\a' -> f a' . toEnum) a . unWrap
{-# INLINE foldlWithKey' #-}

elems :: EnumMap k a -> [a]
elems = I.elems . unWrap
{-# INLINE elems #-}

keys :: (Enum k) => EnumMap k a -> [k]
keys = P.map toEnum . I.keys . unWrap
{-# INLINE keys #-}

keysSet :: (Enum k) => EnumMap k a -> EnumSet k
keysSet = EnumSet.fromDistinctAscList . keys
{-# INLINE keysSet #-}

fromSet :: (Enum k) => (k -> a) -> EnumSet k -> EnumMap k a
fromSet f = EnumMap . I.fromSet (f . toEnum) . EnumSet.enumSetToIntSet
{-# INLINE fromSet #-}

assocs :: (Enum k) => EnumMap k a -> [(k, a)]
assocs = P.map (first toEnum) . I.assocs . unWrap
{-# INLINE assocs #-}

toList :: (Enum k) => EnumMap k a -> [(k, a)]
toList = P.map (first toEnum) . I.toList . unWrap
{-# INLINE toList #-}

toAscList :: (Enum k) => EnumMap k a -> [(k, a)]
toAscList = P.map (first toEnum) . I.toAscList . unWrap
{-# INLINE toAscList #-}

toDescList :: (Enum k) => EnumMap k a -> [(k, a)]
toDescList = P.map (first toEnum) . I.toDescList . unWrap
{-# INLINE toDescList #-}

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
