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
module Truncate.Test.Hexadecimal (suite) where


------------------------------------------------------------------------
import Test.Framework                       ( Test
                                            , testGroup
                                            )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           ( Assertion
                                            , assertFailure
                                            , (@?=)
                                            )
import Test.QuickCheck                      (Positive(..))


------------------------------------------------------------------------
import Test.Truncate.Arbitrary              ( Hexstring32(..)
                                            , Hexstring64(..)
                                            )


------------------------------------------------------------------------
import Truncate


------------------------------------------------------------------------

suite :: Test
suite = testGroup "Hexadecimal"
    [ testProperty "makeHex32" prop_makeHex32
    , testProperty "makeHex64" prop_makeHex64
    , testCase     "makeHex32 with invalid digits" test_makeHex32Invalid
    , testCase     "makeHex64 with invalid digits" test_makeHex64Invalid
    , testProperty "makeHex32 with too many digits" prop_makeHex32TooMany
    , testProperty "makeHex64 with too many digits" prop_makeHex64TooMany
    ]


------------------------------------------------------------------------
-- Constructors with valid inputs

prop_makeHex32 :: Hexstring32 -> Bool
prop_makeHex32 (Hexstring32 h) = case makeHex32 h of
                                     Right (Hex32 x) -> x == h
                                     Right _         -> False
                                     Left  _         -> False

prop_makeHex64 :: Hexstring64 -> Bool
prop_makeHex64 (Hexstring64 h) = case makeHex64 h of
                                     Right (Hex64 x) -> x == h
                                     Right _         -> False
                                     Left  _         -> False


------------------------------------------------------------------------
-- Constructors with invalid inputs

test_makeHex32Invalid :: Assertion
test_makeHex32Invalid = case makeHex32 "g" of
    Left  m -> m @?= "Error: value is not a 32-bit hexadecimal number (invalid digits)"
    Right _ -> assertFailure "A Left value should have been returned"

test_makeHex64Invalid :: Assertion
test_makeHex64Invalid = case makeHex64 "g" of
    Left  m -> m @?= "Error: value is not a 64-bit hexadecimal number (invalid digits)"
    Right _ -> assertFailure "A Left value should have been returned"

prop_makeHex32TooMany :: (Hexstring32, Positive Int) -> Bool
prop_makeHex32TooMany (Hexstring32 b, n) = case makeHex32 tooMany of
    Left  m -> m == "Error: value is not a 32-bit hexadecimal number (too many digits)"
    Right _ -> False
  where
    tooMany = b ++ replicate (getPositive n) '0'

prop_makeHex64TooMany :: (Hexstring64, Positive Int) -> Bool
prop_makeHex64TooMany (Hexstring64 b, n) = case makeHex64 tooMany of
    Left  m -> m == "Error: value is not a 64-bit hexadecimal number (too many digits)"
    Right _ -> False
  where
    tooMany = b ++ replicate (getPositive n) '0'
