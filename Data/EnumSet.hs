{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  $Header$
-- Description :  Data.IntSet with Enum elements.
-- Copyright   :  (c) 2011 Michal Terepeta
-- License     :  BSD3
-- Maintainer  :  michal.terepeta@gmail.com
-- Stability   :  alpha
-- Portability :  uses GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntSet' that allows storing any elements
-- of Enum type class. Useful if one wants to have the performance of
-- 'Data.IntSet' and at the same time use something else than 'Int's (e.g. an
-- 'Int' wrapped with newtype). For documentation see the one for 'Data.IntMap'.

module Data.EnumSet
  ( EnumSet

  -- * Operators
  , (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
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

  -- * Fold
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
  , fromAscList
  , fromDistinctAscList
  ) where

import Prelude hiding ( filter, lookup, map, null )
import qualified Prelude as P

import Data.IntSet ( IntSet )
import qualified Data.IntSet as I

import Data.Monoid ( Monoid )

import Text.Read

-- | Wrapper for 'IntSet' with 'Enum' elements.
newtype EnumSet e = EnumSet { unWrap :: IntSet }
  deriving (Eq, Monoid, Ord)

instance (Enum e, Show e) => Show (EnumSet e) where
  showsPrec p es = showParen (p > 10) $
    showString "fromList " . shows (toList es)

instance (Enum e, Read e) => Read (EnumSet e) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    list <- readPrec
    return (fromList list)

--
-- A few useful functions used through the module; not exported.
--

pairWrap :: (IntSet, IntSet) -> (EnumSet e, EnumSet e)
pairWrap (is1, is2) = (EnumSet is1, EnumSet is2)
{-# INLINE pairWrap #-}

toEnumWrap :: (Enum e) => (Int, IntSet) -> (e, EnumSet e)
toEnumWrap (i, is) = (toEnum i, EnumSet is)
{-# INLINE toEnumWrap #-}

--
-- Here begins the main part.
--

(\\) :: EnumSet e -> EnumSet e -> EnumSet e
(EnumSet is1) \\ (EnumSet is2) = EnumSet $ is1 I.\\ is2
{-# INLINE (\\) #-}

null :: EnumSet e -> Bool
null = I.null . unWrap
{-# INLINE null #-}

size :: EnumSet e -> Int
size = I.size . unWrap
{-# INLINE size #-}

member :: (Enum e) => e -> EnumSet e -> Bool
member e = I.member (fromEnum e) . unWrap
{-# INLINE member #-}

notMember :: (Enum e) => e -> EnumSet e -> Bool
notMember e = I.notMember (fromEnum e) . unWrap
{-# INLINE notMember #-}

empty :: EnumSet e
empty = EnumSet I.empty
{-# INLINE empty #-}

singleton :: (Enum e) => e -> EnumSet e
singleton = EnumSet . I.singleton . fromEnum
{-# INLINE singleton #-}

insert :: (Enum e) => e -> EnumSet e -> EnumSet e
insert e = EnumSet . I.insert (fromEnum e) . unWrap
{-# INLINE insert #-}

delete :: (Enum e) => e -> EnumSet e -> EnumSet e
delete e = EnumSet . I.delete (fromEnum e) . unWrap
{-# INLINE delete #-}

unions :: [EnumSet e] -> EnumSet e
unions = EnumSet . I.unions . P.map unWrap
{-# INLINE unions #-}

union :: EnumSet e -> EnumSet e -> EnumSet e
union (EnumSet is1) (EnumSet is2) = EnumSet $ I.union is1 is2
{-# INLINE union #-}

difference :: EnumSet e -> EnumSet e -> EnumSet e
difference (EnumSet is1) (EnumSet is2) = EnumSet $ I.difference is1 is2
{-# INLINE difference #-}

intersection :: EnumSet e -> EnumSet e -> EnumSet e
intersection (EnumSet is1) (EnumSet is2) = EnumSet $ I.intersection is1 is2
{-# INLINE intersection #-}

isProperSubsetOf :: EnumSet e -> EnumSet e -> Bool
isProperSubsetOf (EnumSet is1) (EnumSet is2) = I.isProperSubsetOf is1 is2
{-# INLINE isProperSubsetOf #-}

isSubsetOf :: EnumSet e -> EnumSet e -> Bool
isSubsetOf (EnumSet is1) (EnumSet is2) = I.isSubsetOf is1 is2
{-# INLINE isSubsetOf #-}

filter :: (Enum e) => (e -> Bool) -> EnumSet e -> EnumSet e
filter f = EnumSet . I.filter (f . toEnum) . unWrap
{-# INLINE filter #-}

partition :: (Enum e) => (e -> Bool) -> EnumSet e -> (EnumSet e, EnumSet e)
partition f = pairWrap . I.partition (f . toEnum) . unWrap
{-# INLINE partition #-}

split :: (Enum e) => e -> EnumSet e -> (EnumSet e, EnumSet e)
split e = pairWrap . I.split (fromEnum e) . unWrap
{-# INLINE split #-}

splitMember :: (Enum e) => e -> EnumSet e -> (EnumSet e, Bool, EnumSet e)
splitMember e =  wrap . I.splitMember (fromEnum e) . unWrap
  where
    wrap (is1, b, is2) = (EnumSet is1, b, EnumSet is2)
{-# INLINE splitMember #-}

maxView :: (Enum e) => EnumSet e -> Maybe (e, EnumSet e)
maxView = fmap toEnumWrap . I.maxView . unWrap
{-# INLINE maxView #-}

minView :: (Enum e) => EnumSet e -> Maybe (e, EnumSet e)
minView = fmap toEnumWrap . I.minView  . unWrap
{-# INLINE minView #-}

deleteFindMin :: (Enum e) => EnumSet e -> (e, EnumSet e)
deleteFindMin = toEnumWrap  . I.deleteFindMin . unWrap
{-# INLINE deleteFindMin #-}

deleteFindMax :: (Enum e) => EnumSet e -> (e, EnumSet e)
deleteFindMax = toEnumWrap . I.deleteFindMax . unWrap
{-# INLINE deleteFindMax #-}

findMin :: (Enum e) => EnumSet e -> e
findMin = toEnum . I.findMin . unWrap
{-# INLINE findMin #-}

findMax :: (Enum e) => EnumSet e -> e
findMax = toEnum . I.findMax . unWrap
{-# INLINE findMax #-}

deleteMin :: EnumSet e -> EnumSet e
deleteMin = EnumSet . I.deleteMin . unWrap
{-# INLINE deleteMin #-}

deleteMax :: EnumSet e -> EnumSet e
deleteMax = EnumSet . I.deleteMax . unWrap
{-# INLINE deleteMax #-}

map :: (Enum e) => (e -> e) -> EnumSet e -> EnumSet e
map f = EnumSet . I.map (fromEnum . f . toEnum) . unWrap
{-# INLINE map #-}

fold :: (Enum e) => (e -> b -> b) -> b -> EnumSet e -> b
fold f acc = I.fold (f . toEnum) acc . unWrap
{-# INLINE fold #-}

elems :: (Enum e) => EnumSet e -> [e]
elems = P.map toEnum . I.elems . unWrap
{-# INLINE elems #-}

toList :: (Enum e) => EnumSet e -> [e]
toList = P.map toEnum . I.toList . unWrap
{-# INLINE toList #-}

toAscList :: (Enum e) => EnumSet e -> [e]
toAscList = P.map toEnum . I.toAscList . unWrap
{-# INLINE toAscList #-}

fromList :: (Enum e) => [e] -> EnumSet e
fromList = EnumSet . I.fromList . P.map fromEnum
{-# INLINE fromList #-}

fromAscList :: (Enum e) => [e] -> EnumSet e
fromAscList = EnumSet . I.fromAscList . P.map fromEnum
{-# INLINE fromAscList #-}

fromDistinctAscList :: (Enum e) => [e] -> EnumSet e
fromDistinctAscList = EnumSet . I.fromDistinctAscList . P.map fromEnum
{-# INLINE fromDistinctAscList #-}
