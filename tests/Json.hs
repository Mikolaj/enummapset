{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Data.Aeson ( decode, encode, FromJSON, ToJSON )
import Data.EnumSet ( EnumSet, fromList )
import Test.HUnit ( Assertion, (@?=) )
import GHC.Generics (Generic)

data Creature = Cat | Dog | Fish | Wolf
  deriving stock (Eq, Show, Enum, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

creature :: [Creature]
creature = [Cat, Dog, Fish, Dog]

creature2 :: [Creature]
creature2 = [Cat, Wolf]

test_encode :: Assertion
test_encode = do
    encode (fromList creature) @?= "[0,1,2]"
    encode (fromList creature2) @?= "[0,3]"

test_decode :: Assertion
test_decode = do
    decode "[0,1,2]" @?=
      (Just (fromList creature) :: Maybe (EnumSet Creature))
    decode "[0,3]" @?=
      (Just (fromList creature2) :: Maybe (EnumSet Creature))
