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
module Truncate.Test.Decimal (suite) where


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

------------------------------------------------------------------------
import Truncate


------------------------------------------------------------------------

suite :: Test
suite = testGroup "Decimal"
    [ testProperty "makeDec32" prop_makeDec32
    , testProperty "makeDec64" prop_makeDec64
    , testCase     "makeDec32 with invalid input" test_makeDec32Invalid
    , testCase     "makeDec64 with invalid input" test_makeDec64Invalid
    ]


------------------------------------------------------------------------
-- Constructors with valid inputs

prop_makeDec32 :: Float -> Bool
prop_makeDec32 f = case makeDec32 (show f) of
                       Right (Dec32 x) -> x == f
                       Right _         -> False
                       Left  _         -> False

prop_makeDec64 :: Double -> Bool
prop_makeDec64 d = case makeDec64 (show d) of
                       Right (Dec64 x) -> x == d
                       Right _         -> False
                       Left  _         -> False


------------------------------------------------------------------------
-- Constructors with invalid inputs

test_makeDec32Invalid :: Assertion
test_makeDec32Invalid = case makeDec32 "3.14.15" of
    Left  m -> m @?= "Error: value is not a 32-bit decimal number"
    Right _ -> assertFailure "A Left value should have been returned"

test_makeDec64Invalid :: Assertion
test_makeDec64Invalid = case makeDec64 "3.14.15" of
    Left  m -> m @?= "Error: value is not a 64-bit decimal number"
    Right _ -> assertFailure "A Left value should have been returned"
