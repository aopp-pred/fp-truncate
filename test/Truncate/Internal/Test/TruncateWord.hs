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
module Truncate.Internal.Test.TruncateWord (suite) where


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
import Data.Bits                            (testBit)
import Data.Word                            ( Word32
                                            , Word64
                                            )


------------------------------------------------------------------------
import Test.Truncate.Arbitrary              ( TiedWord32(..)
                                            , TiedWord64(..)
                                            )


------------------------------------------------------------------------
import Truncate.Internal


------------------------------------------------------------------------

suite :: Test
suite = testGroup "truncateWord"
    [ testProperty "No-op truncation [32-bit]"             prop_null32
    , testProperty "No-op truncation [64-bit]"             prop_null64
    , testCase     "Truncation of zero is zero [32-bit]"   test_zero32
    , testCase     "Truncation of zero is zero [64-bit]"   test_zero64
    , testProperty "Truncation reduces precision [32-bit]" prop_trunc32
    , testProperty "Truncation reduces precision [64-bit]" prop_trunc64
    , testProperty "Tie-to-even [32-bit]"                  prop_tied32
    , testProperty "Tie-to-even [64-bit]"                  prop_tied64
    ]


------------------------------------------------------------------------
-- No-op truncations

prop_null32 :: Word32 -> Bool
prop_null32 w = truncateWord w' 23 == w'
  where
    w' = WordF32 w

prop_null64 :: Word64 -> Bool
prop_null64 w = truncateWord w' 52 == w'
  where
    w' = WordF64 w

test_zero32 :: Assertion
test_zero32 = all (== z) (map (truncateWord z) [0..23]) @?= True
  where
    z = WordF32 (makeZero :: Word32)

test_zero64 :: Assertion
test_zero64 = all (== z) (map (truncateWord z) [0..52]) @?= True
  where
    z = WordF64 (makeZero :: Word64)

makeZero :: Integral a => a
makeZero = fromIntegral (0 :: Integer)

------------------------------------------------------------------------
-- Truncating should reduce or retain precision

prop_trunc32 :: Word32 -> Bool
prop_trunc32 w = checkTruncation (WordF32 w) 23

prop_trunc64 :: Word64 -> Bool
prop_trunc64 w = checkTruncation (WordF64 w) 52

checkTruncation :: WordF -> Int -> Bool
checkTruncation w sbits = and [ rightZeroBits (truncateWord w n) >= (sbits - n)
                              | n <- [0..sbits]
                              ]
  where
    rightZeroBits (WordF32 b) = rightZeroBits' b 32
    rightZeroBits (WordF64 b) = rightZeroBits' b 64
    rightZeroBits' b m        = length $ takeWhile (not . testBit b) [0..m]


------------------------------------------------------------------------
-- Numbers halfway between two representations should always round to
-- even (0 in last retained bit)
--
-- For a 32-bit input, truncating to n bits should yield a zero in bits
-- (23 - n) as shown below:
-- For a 32-bit input, truncating at bit n should yield a zero in bit
-- (n + 1) as shown below:
--                                                   (23-n):=0
--                                                       |
-- S E E E E E E E E M M M M M M M M M M M M M M M M M M M 1 0 0 0
--                   |___________________________________|
--                                     |
--                        n retained significand bits

prop_tied32 :: TiedWord32 -> Bool
prop_tied32 (TiedWord32 (w, n)) = isUnset truncated (23 - n)
  where
    truncated           = truncateWord (WordF32 w) n

prop_tied64 :: TiedWord64 -> Bool
prop_tied64 (TiedWord64 (w, n)) = isUnset truncated (52 - n)
  where
    truncated           = truncateWord (WordF64 w) n

isUnset :: WordF -> Int -> Bool
isUnset (WordF32 b) = not . testBit b
isUnset (WordF64 b) = not . testBit b
