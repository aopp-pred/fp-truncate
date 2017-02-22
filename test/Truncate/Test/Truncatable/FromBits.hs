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
module Truncate.Test.Truncatable.FromBits (suite) where


------------------------------------------------------------------------
import Test.Framework                       ( Test
                                            , testGroup
                                            )
import Test.Framework.Providers.QuickCheck2 (testProperty)


------------------------------------------------------------------------
import Data.Word                            ( Word32
                                            , Word64
                                            )


------------------------------------------------------------------------
import Truncate


------------------------------------------------------------------------

suite :: Test
suite = testGroup "truncatable fromBits"
    [ testProperty "Bin32 round-trip from bits" prop_roundTripBin32
    , testProperty "Bin64 round-trip from bits" prop_roundTripBin64
    , testProperty "Dec32 round-trip from bits" prop_roundTripDec32
    , testProperty "Dec64 round-trip from bits" prop_roundTripDec64
    , testProperty "Hex32 round-trip from bits" prop_roundTripHex32
    , testProperty "Hex64 round-trip from bits" prop_roundTripHex64
    ]


------------------------------------------------------------------------

prop_roundTripBin32 :: Word32 -> Bool
prop_roundTripBin32 w = makeBits (fromBits w' :: Binary) == w'
  where
    w' = WordF32 w

prop_roundTripBin64 :: Word64 -> Bool
prop_roundTripBin64 w = makeBits (fromBits w' :: Binary) == w'
  where
    w' = WordF64 w

prop_roundTripDec32 :: Word32 -> Bool
prop_roundTripDec32 w = makeBits (fromBits w' :: Decimal) == w'
  where
    w' = WordF32 w

prop_roundTripDec64 :: Word64 -> Bool
prop_roundTripDec64 w = makeBits (fromBits w' :: Decimal) == w'
  where
    w' = WordF64 w

prop_roundTripHex32 :: Word32 -> Bool
prop_roundTripHex32 w = makeBits (fromBits w' :: Hexadecimal) == w'
  where
    w' = WordF32 w

prop_roundTripHex64 :: Word64 -> Bool
prop_roundTripHex64 w = makeBits (fromBits w' :: Hexadecimal) == w'
  where
    w' = WordF64 w
