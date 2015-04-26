-- |
-- Module      :  $Header$
-- Description :  Data.IntMap with Enum keys.
-- Copyright   :  (c) 2011-2013 Michal Terepeta
-- License     :  BSD3
-- Maintainer  :  michal.terepeta@gmail.com
-- Stability   :  alpha
-- Portability :  uses DeriveDataTypeable and GeneralizedNewtypeDeriving

-- This is a simple wrapper for 'Data.IntMap' that works with any type of keys
-- that are instances of 'Enum' type class.  For documentation please see the
-- one for 'Data.IntMap'.

module Data.EnumMap
  ( module Data.EnumMap.Lazy
  , insertWith'
  , insertWithKey'
  , fold
  , foldWithKey
  ) where

import Prelude hiding ( filter, foldr, foldl, lookup, map, null )

import Data.EnumMap.Lazy
import Data.EnumMap.Base

import qualified Data.IntMap as I

insertWith' :: (Enum k) => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith' f k x = EnumMap . I.insertWith' f (fromEnum k) x . unWrap

insertWithKey' :: (Enum k)
  => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey' f k x =
  EnumMap . I.insertWithKey' (f . toEnum) (fromEnum k) x . unWrap

fold :: (a -> b -> b) -> b -> EnumMap k a -> b
fold f a = I.fold f a . unWrap

foldWithKey :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey f a = I.foldWithKey (f . toEnum) a . unWrap
