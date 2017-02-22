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
module Truncate.Test.Truncatable.Convert (suite) where


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
suite = testGroup "Truncatable convert"
    [ testProperty "Convert Bin32 -> Dec32" prop_roundTripBin32Dec32
    , testProperty "Convert Bin32 -> Hex32" prop_roundTripBin32Hex32
    , testProperty "Convert Dec32 -> Bin32" prop_roundTripDec32Bin32
    , testProperty "Convert Dec32 -> Hex32" prop_roundTripDec32Hex32
    , testProperty "Convert Hex32 -> Bin32" prop_roundTripHex32Bin32
    , testProperty "Convert Hex32 -> Dec32" prop_roundTripHex32Dec32
    , testProperty "Convert Bin64 -> Dec64" prop_roundTripBin64Dec64
    , testProperty "Convert Bin64 -> Hex64" prop_roundTripBin64Hex64
    , testProperty "Convert Dec64 -> Bin64" prop_roundTripDec64Bin64
    , testProperty "Convert Dec64 -> Hex64" prop_roundTripDec64Hex64
    , testProperty "Convert Hex64 -> Bin64" prop_roundTripHex64Bin64
    , testProperty "Convert Hex64 -> Dec64" prop_roundTripHex64Dec64
    ]


------------------------------------------------------------------------
-- 32-bit tests

prop_roundTripBin32Dec32 :: Word32 -> Bool
prop_roundTripBin32Dec32 b = (toBin . toDec)  bin == bin
  where
    bin = fromBits (WordF32 b) :: Binary

prop_roundTripBin32Hex32 :: Word32 -> Bool
prop_roundTripBin32Hex32 b = (toBin . toHex) bin == bin
  where
    bin = fromBits (WordF32 b) :: Binary

prop_roundTripDec32Bin32 :: Word32 -> Bool
prop_roundTripDec32Bin32 b = case isNaN' dec of
                                 True  -> isNaN' roundTrip
                                 False -> roundTrip == dec
  where
    dec       = fromBits (WordF32 b) :: Decimal
    roundTrip = toDec . toBin $ dec

prop_roundTripDec32Hex32 :: Word32 -> Bool
prop_roundTripDec32Hex32 b = case isNaN' dec of
                                 True  -> isNaN' roundTrip
                                 False -> roundTrip == dec
  where
    dec       = fromBits (WordF32 b) :: Decimal
    roundTrip = toDec . toHex $ dec

prop_roundTripHex32Bin32 :: Word32 -> Bool
prop_roundTripHex32Bin32 b = (toHex . toBin) hex == hex
  where
    hex = fromBits (WordF32 b) :: Hexadecimal

prop_roundTripHex32Dec32 :: Word32 -> Bool
prop_roundTripHex32Dec32 b = (toHex . toDec) hex == hex
  where
    hex = fromBits (WordF32 b) :: Hexadecimal


------------------------------------------------------------------------
-- 64-bit tests

prop_roundTripBin64Dec64 :: Word64 -> Bool
prop_roundTripBin64Dec64 b = (toBin . toDec) bin == bin
  where
    bin = fromBits (WordF64 b) :: Binary

prop_roundTripBin64Hex64 :: Word64 -> Bool
prop_roundTripBin64Hex64 b = (toBin . toHex) bin == bin
  where
    bin = fromBits (WordF64 b) :: Binary

prop_roundTripDec64Bin64 :: Word64 -> Bool
prop_roundTripDec64Bin64 b = case isNaN' dec of
                                 True  -> isNaN' roundTrip
                                 False -> roundTrip == dec
  where
    dec       = fromBits (WordF64 b) :: Decimal
    roundTrip = toDec . toBin $ dec

prop_roundTripDec64Hex64 :: Word64 -> Bool
prop_roundTripDec64Hex64 b = case isNaN' dec of
                                 True  -> isNaN' roundTrip
                                 False -> roundTrip == dec
  where
    dec       = fromBits (WordF64 b) :: Decimal
    roundTrip = toDec . toHex $ dec

prop_roundTripHex64Bin64 :: Word64 -> Bool
prop_roundTripHex64Bin64 b = (toHex . toBin) hex == hex
  where
    hex = fromBits (WordF64 b) :: Hexadecimal

prop_roundTripHex64Dec64 :: Word64 -> Bool
prop_roundTripHex64Dec64 b = (toHex . toDec) hex == hex
  where
    hex = fromBits (WordF64 b) :: Hexadecimal


------------------------------------------------------------------------

isNaN' :: Decimal -> Bool
isNaN' (Dec32 v) = isNaN v
isNaN' (Dec64 v) = isNaN v
