--
-- Copyright 2017 Andrew Dawson
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
module Truncate.Test.Truncatable.MakeBits (suite) where


------------------------------------------------------------------------
import Test.Framework                       ( Test
                                            , testGroup
                                            )
import Test.Framework.Providers.QuickCheck2 (testProperty)


------------------------------------------------------------------------
import Test.Truncate.Arbitrary              ( Bitstring32(..)
                                            , Bitstring64(..)
                                            , Hexstring32(..)
                                            , Hexstring64(..)
                                            )

------------------------------------------------------------------------
import Truncate


------------------------------------------------------------------------

suite :: Test
suite = testGroup "Truncatable makeBits"
    [ testProperty "Bin32 round-trip to bits" prop_roundTripBin32
    , testProperty "Bin64 round-trip to bits" prop_roundTripBin64
    , testProperty "Dec32 round-trip to bits" prop_roundTripDec32
    , testProperty "Dec64 round-trip to bits" prop_roundTripDec64
    , testProperty "Hex32 round-trip to bits" prop_roundTripHex32
    , testProperty "Hex64 round-trip to bits" prop_roundTripHex64
    ]


------------------------------------------------------------------------

prop_roundTripBin32 :: Bitstring32 -> Bool
prop_roundTripBin32 (Bitstring32 bs) = fromBits (makeBits b) == b
  where
    b = Bin32 bs

prop_roundTripBin64 :: Bitstring64 -> Bool
prop_roundTripBin64 (Bitstring64 bs) = fromBits (makeBits b) == b
  where
    b = Bin64 bs

prop_roundTripDec32 :: Float -> Bool
prop_roundTripDec32 f = fromBits (makeBits d) == d
  where
    d = Dec32 f

prop_roundTripDec64 :: Double -> Bool
prop_roundTripDec64 f = fromBits (makeBits d) == d
  where
    d = Dec64 f

prop_roundTripHex32 :: Hexstring32 -> Bool
prop_roundTripHex32 (Hexstring32 hs) = fromBits (makeBits h) == h
  where
    h = Hex32 hs

prop_roundTripHex64 :: Hexstring64 -> Bool
prop_roundTripHex64 (Hexstring64 hs) = fromBits (makeBits h) == h
  where
    h = Hex64 hs
