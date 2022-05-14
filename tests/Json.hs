{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Json where

import Data.Aeson ( decode, encode, FromJSON, ToJSON )
import Data.EnumSet ( EnumSet)
import Data.EnumMap ( EnumMap)
import qualified Data.EnumSet as ES
import qualified Data.EnumMap as EM
import Test.HUnit ( Assertion, (@?=) )
import GHC.Generics (Generic)

data Creature = Cat | Dog | Fish | Wolf
  deriving stock (Eq, Show, Enum, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

creature :: [Creature]
creature = [Cat, Dog, Fish, Dog]

creature2 :: [Creature]
creature2 = [Cat, Wolf]

creatureM :: [(Creature,String)]
creatureM = [(Cat,"C"), (Dog,"D")]

creatureMRev :: [(Creature,String)]
creatureMRev = [(Dog,"D"), (Cat,"C")]

creatureMDup :: [(Creature, String)]
creatureMDup = [(Cat,"C"), (Dog,"D"),(Dog,"DD")]

test_encode_set :: Assertion
test_encode_set = do
    encode (ES.fromList creature) @?= "[0,1,2]"
    encode (ES.fromList creature2) @?= "[0,3]"

test_encode_map :: Assertion
test_encode_map = do
    encode (EM.fromSet id $ ES.fromList creature)
      @?= "[[0,\"Cat\"],[1,\"Dog\"],[2,\"Fish\"]]"
    encode (EM.fromSet id $ ES.fromList creature2) @?= "[[0,\"Cat\"],[3,\"Wolf\"]]"
    encode (EM.fromList creatureM) @?= "[[0,\"C\"],[1,\"D\"]]"
    encode (EM.fromList creatureMRev) @?= "[[0,\"C\"],[1,\"D\"]]"
    encode (EM.fromList creatureMDup) @?= "[[0,\"C\"],[1,\"DD\"]]"

test_decode :: Assertion
test_decode = do
    decode "[0,1,2]" @?=
      (Just (ES.fromList creature) :: Maybe (EnumSet Creature))
    decode "[0,3]" @?=
      (Just (ES.fromList creature2) :: Maybe (EnumSet Creature))

test_decode_map :: Assertion
test_decode_map = do
    decode "[[0,\"Cat\"],[1,\"Dog\"],[2,\"Fish\"]]" @?=
      ((Just $ EM.fromSet id $ ES.fromList creature) :: Maybe (EnumMap Creature Creature))
    (EM.keys <$> decode @(EnumMap Creature Creature) "[[0,\"Cat\"],[1,\"Dog\"],[2,\"Fish\"]]")
      @?= Just [Cat,Dog,Fish]
    EM.toList <$> decode "[[0,\"C\"],[1,\"D\"]]" @?=
      (Just creatureM :: Maybe [(Creature, String)])
