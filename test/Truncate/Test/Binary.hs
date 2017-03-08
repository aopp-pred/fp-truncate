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
module Truncate.Test.Binary (suite) where


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
import Test.Truncate.Arbitrary              ( Bitstring32(..)
                                            , Bitstring64(..)
                                            )


------------------------------------------------------------------------
import Truncate


------------------------------------------------------------------------

suite :: Test
suite = testGroup "Binary"
    [ testProperty "makeBin32" prop_makeBin32
    , testProperty "makeBin64" prop_makeBin64
    , testCase     "makeBin32 with invalid digits" test_makeBin32Invalid
    , testCase     "makeBin64 with invalid digits" test_makeBin64Invalid
    , testProperty "makeBin32 with too many digits" prop_makeBin32TooMany
    , testProperty "makeBin64 with too many digits" prop_makeBin64TooMany
    ]


------------------------------------------------------------------------
-- Constructors with valid inputs

prop_makeBin32 :: Bitstring32 -> Bool
prop_makeBin32 (Bitstring32 b) = case makeBin32 b of
                                     Right (Bin32 x) -> x == b
                                     Right _         -> False
                                     Left  _         -> False

prop_makeBin64 :: Bitstring64 -> Bool
prop_makeBin64 (Bitstring64 b) = case makeBin64 b of
                                     Right (Bin64 x) -> x == b
                                     Right _         -> False
                                     Left  _         -> False


------------------------------------------------------------------------
-- Constructors with invalid inputs

test_makeBin32Invalid :: Assertion
test_makeBin32Invalid = case makeBin32 "2" of
    Left  m -> m @?= "Error: value is not a 32-bit binary number (invalid digits)"
    Right _ -> assertFailure "A Left value should have been returned"

test_makeBin64Invalid :: Assertion
test_makeBin64Invalid = case makeBin64 "2" of
    Left  m -> m @?= "Error: value is not a 64-bit binary number (invalid digits)"
    Right _ -> assertFailure "A Left value should have been returned"

prop_makeBin32TooMany :: (Bitstring32, Positive Int) -> Bool
prop_makeBin32TooMany (Bitstring32 b, n) = case makeBin32 tooMany of
    Left  m -> m == "Error: value is not a 32-bit binary number (too many digits)"
    Right _ -> False
  where
    tooMany = b ++ replicate (getPositive n) '0'

prop_makeBin64TooMany :: (Bitstring64, Positive Int) -> Bool
prop_makeBin64TooMany (Bitstring64 b, n) = case makeBin64 tooMany of
    Left  m -> m == "Error: value is not a 64-bit binary number (too many digits)"
    Right _ -> False
  where
    tooMany = b ++ replicate (getPositive n) '0'
