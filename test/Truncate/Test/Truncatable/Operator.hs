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
module Truncate.Test.Truncatable.Operator (suite) where


------------------------------------------------------------------------
import Test.Framework                       ( Test
                                            , testGroup
                                            )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           ( (@?=)
                                            , Assertion
                                            )

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
suite = testGroup "Truncatable (<#)"
    [ testProperty "Bin32 identity truncation" prop_idBin32
    , testProperty "Bin64 identity truncation" prop_idBin64
    , testProperty "Hex32 identity truncation" prop_idHex32
    , testProperty "Hex64 identity truncation" prop_idHex64
    , testProperty "Dec32 identity truncation" prop_idDec32
    , testProperty "Dec64 identity truncation" prop_idDec64
    , testCase     "Bin32 zero truncation"     test_zeroBin32
    , testCase     "Bin64 zero truncation"     test_zeroBin64
    , testCase     "Dec32 zero truncation"     test_zeroDec32
    , testCase     "Dec64 zero truncation"     test_zeroDec64
    , testCase     "Hex32 zero truncation"     test_zeroHex32
    , testCase     "Hex64 zero truncation"     test_zeroHex64
    ]


------------------------------------------------------------------------
-- Truncating to the full significand size does not change the input

prop_idBin32 :: Bitstring32 -> Bool
prop_idBin32 (Bitstring32 b) = b' <# 23 == b'
  where
    b' = Bin32 b

prop_idBin64 :: Bitstring64 -> Bool
prop_idBin64 (Bitstring64 b) = b' <# 52 == b'
  where
    b' = Bin64 b

prop_idDec32 :: Float -> Bool
prop_idDec32 f = case isNaN f of
                     True  -> isNaN $ (\ (Dec32 x) -> x) (f' <# 23)
                     False -> f' <# 23 == f'
  where
    f' = Dec32 f

prop_idDec64 :: Double -> Bool
prop_idDec64 d = case isNaN d of
                     True  -> isNaN $ (\ (Dec64 x) -> x) (d' <# 52)
                     False -> d' <# 52 == d'
  where
    d' = Dec64 d

prop_idHex32 :: Hexstring32 -> Bool
prop_idHex32 (Hexstring32 h) = h' <# 23 == h'
  where
    h' = Hex32 h

prop_idHex64 :: Hexstring64 -> Bool
prop_idHex64 (Hexstring64 h) = h' <# 52 == h'
  where
    h' = Hex64 h


------------------------------------------------------------------------
-- Truncating zero always gives zero

test_zeroBin32 :: Assertion
test_zeroBin32 = all (== b) (map (b <#) [0..23]) @?= True
  where
    b = Bin32 "0"

test_zeroBin64 :: Assertion
test_zeroBin64 = all (== b) (map (b <#) [0..52]) @?= True
  where
    b = Bin64 "0"

test_zeroDec32 :: Assertion
test_zeroDec32 = all (== d) (map (d <#) [0..23]) @?= True
  where
    d = Dec32 0

test_zeroDec64 :: Assertion
test_zeroDec64 = all (== d) (map (d <#) [0..52]) @?= True
  where
    d = Dec64 0

test_zeroHex32 :: Assertion
test_zeroHex32 = all (== h) (map (h <#) [0..23]) @?= True
  where
    h = Hex32 "0"

test_zeroHex64 :: Assertion
test_zeroHex64 = all (== h) (map (h <#) [0..52]) @?= True
  where
    h = Hex64 "0"
