{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  $Header$
-- Description :  Data.IntSet with Enum elements.
-- Copyright   :  (c) 2011-2019 Michal Terepeta
-- License     :  BSD3
-- Maintainer  :  mikolaj.konarski@funktory.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntSet' that allows storing any elements
-- of Enum type class. Useful if one wants to have the performance of
-- 'Data.IntSet' and at the same time use something else than 'Int's (e.g. an
-- 'Int' wrapped with newtype). For documentation see the one for 'Data.IntSet'.

module Data.EnumSet
  ( EnumSet

  -- * Wrapping/unwrapping
  , intSetToEnumSet
  , enumSetToIntSet

  -- * Operators
  , (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , isSubsetOf
  , isProperSubsetOf

  -- * Construction
  , empty
  , singleton
  , insert
  , delete

  -- * Combine
  , union
  , unions
  , difference
  , intersection

  -- * Filter
  , filter
  , partition
  , split
  , splitMember

  -- * Map
  , map

  -- * Folds
  , foldr
  , foldl
  -- ** Strict folds
  , foldr'
  , foldl'
  -- ** Legacy folds
  , fold


  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , maxView
  , minView

  -- * Conversion

  -- ** List
  , elems
  , toList
  , fromList

  -- ** Ordered list
  , toAscList
  , toDescList
  , fromAscList
  , fromDistinctAscList

  ) where

import Prelude hiding ( filter, foldl, foldr, lookup, map, null )
import qualified Prelude as P

import Data.IntSet ( IntSet )
import qualified Data.IntSet as I

import Control.Arrow ( (***) )
import Control.DeepSeq ( NFData )
import Data.Monoid ( Monoid )
import Data.Semigroup ( Semigroup )
import Data.Typeable ( Typeable )

import Text.Read

-- | Wrapper for 'IntSet' with 'Enum' elements.
newtype EnumSet k = EnumSet { unWrap :: IntSet }
  deriving (Eq, Semigroup, Monoid, Ord, Typeable, NFData)

instance (Enum k, Show k) => Show (EnumSet k) where
  showsPrec p ks = showParen (p > 10) $
    showString "fromList " . shows (toList ks)

instance (Enum k, Read k) => Read (EnumSet k) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    list <- readPrec
    return (fromList list)

--
-- Conversion to/from 'IntSet'.
--

-- | Wrap 'IntSet'.
intSetToEnumSet :: IntSet -> EnumSet k
intSetToEnumSet = EnumSet
{-# INLINE intSetToEnumSet #-}

-- | Unwrap 'IntSet'.
enumSetToIntSet :: EnumSet k -> IntSet
enumSetToIntSet = unWrap
{-# INLINE enumSetToIntSet #-}

--
-- Here begins the main part.
--

(\\) :: EnumSet k -> EnumSet k -> EnumSet k
(EnumSet is1) \\ (EnumSet is2) = EnumSet $ is1 I.\\ is2
{-# INLINE (\\) #-}

null :: EnumSet k -> Bool
null = I.null . unWrap
{-# INLINE null #-}

size :: EnumSet k -> Int
size = I.size . unWrap
{-# INLINE size #-}

member :: (Enum k) => k -> EnumSet k -> Bool
member k = I.member (fromEnum k) . unWrap
{-# INLINE member #-}

notMember :: (Enum k) => k -> EnumSet k -> Bool
notMember k = I.notMember (fromEnum k) . unWrap
{-# INLINE notMember #-}

lookupLT :: (Enum k) => k -> EnumSet k -> Maybe k
lookupLT k = fmap toEnum . I.lookupLT (fromEnum k) . unWrap
{-# INLINE lookupLT #-}

lookupGT :: (Enum k) => k -> EnumSet k -> Maybe k
lookupGT k = fmap toEnum . I.lookupGT (fromEnum k) . unWrap
{-# INLINE lookupGT #-}

lookupLE :: (Enum k) => k -> EnumSet k -> Maybe k
lookupLE k = fmap toEnum . I.lookupLE (fromEnum k) . unWrap
{-# INLINE lookupLE #-}

lookupGE :: (Enum k) => k -> EnumSet k -> Maybe k
lookupGE k = fmap toEnum . I.lookupGE (fromEnum k) . unWrap
{-# INLINE lookupGE #-}

empty :: EnumSet k
empty = EnumSet I.empty
{-# INLINE empty #-}

singleton :: (Enum k) => k -> EnumSet k
singleton = EnumSet . I.singleton . fromEnum
{-# INLINE singleton #-}

insert :: (Enum k) => k -> EnumSet k -> EnumSet k
insert k = EnumSet . I.insert (fromEnum k) . unWrap
{-# INLINE insert #-}

delete :: (Enum k) => k -> EnumSet k -> EnumSet k
delete k = EnumSet . I.delete (fromEnum k) . unWrap
{-# INLINE delete #-}

unions :: [EnumSet k] -> EnumSet k
unions = EnumSet . I.unions . P.map unWrap
{-# INLINE unions #-}

union :: EnumSet k -> EnumSet k -> EnumSet k
union (EnumSet is1) (EnumSet is2) = EnumSet $ I.union is1 is2
{-# INLINE union #-}

difference :: EnumSet k -> EnumSet k -> EnumSet k
difference (EnumSet is1) (EnumSet is2) = EnumSet $ I.difference is1 is2
{-# INLINE difference #-}

intersection :: EnumSet k -> EnumSet k -> EnumSet k
intersection (EnumSet is1) (EnumSet is2) = EnumSet $ I.intersection is1 is2
{-# INLINE intersection #-}

isProperSubsetOf :: EnumSet k -> EnumSet k -> Bool
isProperSubsetOf (EnumSet is1) (EnumSet is2) = I.isProperSubsetOf is1 is2
{-# INLINE isProperSubsetOf #-}

isSubsetOf :: EnumSet k -> EnumSet k -> Bool
isSubsetOf (EnumSet is1) (EnumSet is2) = I.isSubsetOf is1 is2
{-# INLINE isSubsetOf #-}

filter :: (Enum k) => (k -> Bool) -> EnumSet k -> EnumSet k
filter f = EnumSet . I.filter (f . toEnum) . unWrap
{-# INLINE filter #-}

partition :: (Enum k) => (k -> Bool) -> EnumSet k -> (EnumSet k, EnumSet k)
partition f = (EnumSet *** EnumSet) . I.partition (f . toEnum) . unWrap
{-# INLINE partition #-}

split :: (Enum k) => k -> EnumSet k -> (EnumSet k, EnumSet k)
split k = (EnumSet *** EnumSet) . I.split (fromEnum k) . unWrap
{-# INLINE split #-}

splitMember :: (Enum k) => k -> EnumSet k -> (EnumSet k, Bool, EnumSet k)
splitMember k =  wrap . I.splitMember (fromEnum k) . unWrap
  where
    wrap (is1, b, is2) = (EnumSet is1, b, EnumSet is2)
{-# INLINE splitMember #-}

maxView :: (Enum k) => EnumSet k -> Maybe (k, EnumSet k)
maxView = fmap (toEnum *** EnumSet) . I.maxView . unWrap
{-# INLINE maxView #-}

minView :: (Enum k) => EnumSet k -> Maybe (k, EnumSet k)
minView = fmap (toEnum *** EnumSet) . I.minView  . unWrap
{-# INLINE minView #-}

deleteFindMin :: (Enum k) => EnumSet k -> (k, EnumSet k)
deleteFindMin = (toEnum *** EnumSet) . I.deleteFindMin . unWrap
{-# INLINE deleteFindMin #-}

deleteFindMax :: (Enum k) => EnumSet k -> (k, EnumSet k)
deleteFindMax = (toEnum *** EnumSet) . I.deleteFindMax . unWrap
{-# INLINE deleteFindMax #-}

findMin :: (Enum k) => EnumSet k -> k
findMin = toEnum . I.findMin . unWrap
{-# INLINE findMin #-}

findMax :: (Enum k) => EnumSet k -> k
findMax = toEnum . I.findMax . unWrap
{-# INLINE findMax #-}

deleteMin :: EnumSet k -> EnumSet k
deleteMin = EnumSet . I.deleteMin . unWrap
{-# INLINE deleteMin #-}

deleteMax :: EnumSet k -> EnumSet k
deleteMax = EnumSet . I.deleteMax . unWrap
{-# INLINE deleteMax #-}

map :: (Enum k) => (k -> k) -> EnumSet k -> EnumSet k
map f = EnumSet . I.map (fromEnum . f . toEnum) . unWrap
{-# INLINE map #-}

foldr :: (Enum k) => (k -> b -> b) -> b -> EnumSet k -> b
foldr f acc = I.foldr (f . toEnum) acc . unWrap
{-# INLINE foldr #-}

foldl :: (Enum k) => (a -> k -> a) -> a -> EnumSet k -> a
foldl f acc = I.foldl (\a -> f a . toEnum) acc . unWrap
{-# INLINE foldl #-}

foldr' :: (Enum k) => (k -> b -> b) -> b -> EnumSet k -> b
foldr' f acc = I.foldr' (f . toEnum) acc . unWrap
{-# INLINE foldr' #-}

foldl' :: (Enum k) => (a -> k -> a) -> a -> EnumSet k -> a
foldl' f acc = I.foldl' (\a -> f a . toEnum) acc . unWrap
{-# INLINE foldl' #-}

fold :: (Enum k) => (k -> b -> b) -> b -> EnumSet k -> b
fold f acc = I.fold (f . toEnum) acc . unWrap
{-# INLINE fold #-}

elems :: (Enum k) => EnumSet k -> [k]
elems = P.map toEnum . I.elems . unWrap
{-# INLINE elems #-}

toList :: (Enum k) => EnumSet k -> [k]
toList = P.map toEnum . I.toList . unWrap
{-# INLINE toList #-}

toAscList :: (Enum k) => EnumSet k -> [k]
toAscList = P.map toEnum . I.toAscList . unWrap
{-# INLINE toAscList #-}

toDescList :: (Enum k) => EnumSet k -> [k]
toDescList = P.map toEnum . I.toDescList . unWrap
{-# INLINE toDescList #-}

fromList :: (Enum k) => [k] -> EnumSet k
fromList = EnumSet . I.fromList . P.map fromEnum
{-# INLINE fromList #-}

fromAscList :: (Enum k) => [k] -> EnumSet k
fromAscList = EnumSet . I.fromAscList . P.map fromEnum
{-# INLINE fromAscList #-}

fromDistinctAscList :: (Enum k) => [k] -> EnumSet k
fromDistinctAscList = EnumSet . I.fromDistinctAscList . P.map fromEnum
{-# INLINE fromDistinctAscList #-}
