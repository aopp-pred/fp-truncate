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
module Test.Truncate.Arbitrary
    ( Bitstring32(..)
    , Bitstring64(..)
    , Hexstring32(..)
    , Hexstring64(..)
    , TiedWord32(..)
    , TiedWord64(..)
    ) where


------------------------------------------------------------------------
import Test.QuickCheck


------------------------------------------------------------------------
import Data.Bits
import Data.Word


------------------------------------------------------------------------
-- 'Arbitrary' instances for bit-strings representing floating-point
-- numbers

newtype Bitstring32 = Bitstring32 String
                    deriving (Show)

instance Arbitrary Bitstring32 where
    arbitrary = Bitstring32 <$> genBitstring 32

newtype Bitstring64 = Bitstring64 String
                    deriving (Show)

instance Arbitrary Bitstring64 where
    arbitrary = Bitstring64 <$> genBitstring 64

genBitstring :: Int -> Gen String
genBitstring n = (listOf . choose) ('0', '1')
                 `suchThat` \ s -> length s == n


------------------------------------------------------------------------
-- 'Arbitrary' instances for hexadecimal-strings representing floating-point
-- numbers

newtype Hexstring32 = Hexstring32 String
                    deriving (Show)

instance Arbitrary Hexstring32 where
    arbitrary = Hexstring32 <$> genHexstring 8

newtype Hexstring64 = Hexstring64 String
                    deriving (Show)

instance Arbitrary Hexstring64 where
    arbitrary = Hexstring64 <$> genHexstring 16

genHexstring :: Int -> Gen String
genHexstring n = (listOf . elements) "0123456789abcdef"
                 `suchThat` \ s -> length s == n


------------------------------------------------------------------------
-- 'Arbitrary' instances for 'Word32' and 'Word64' types that produce
-- a number with a '1' in a particular bit and '0's in the bits to then
-- right of this

newtype TiedWord32 = TiedWord32 (Word32, Int)
                   deriving (Show)

instance Arbitrary TiedWord32 where
    arbitrary = TiedWord32 <$> genTiedBits32

genTiedBits32 :: Gen (Word32, Int)
genTiedBits32 = do
    n <- choose (1, 22)
    w <- arbitrary :: Gen Word32
    return (tieBits w (22 - n), n)

newtype TiedWord64 = TiedWord64 (Word64, Int)
                   deriving (Show)

instance Arbitrary TiedWord64 where
    arbitrary = TiedWord64 <$> genTiedBits64

genTiedBits64 :: Gen (Word64, Int)
genTiedBits64 = do
    n <- choose (1, 51)
    w <- arbitrary :: Gen Word64
    return (tieBits w (51 - n), n)

tieBits :: Bits a => a -> Int -> a
tieBits bits n = setBit (dropBits n bits) n
  where
    dropBits m = (`shiftL` m) . (`shiftR` m)
