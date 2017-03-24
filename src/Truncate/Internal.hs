{- |
Description : Truncation of floating-point significands
Copyright   : 2017 Andrew Dawson
License     : Apache-2.0

Tools for truncating the number of bits in the significand of floating-point
numbers. The numbers can be represented in decimal, binary or hexadecimal.
You can create additional representations by making instances of the
'Truncatable' typeclass.
-}
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
module Truncate.Internal where


------------------------------------------------------------------------
import Data.Bits
import Data.Word
import Data.Char ( digitToInt
                 , intToDigit
                 )
import Numeric   ( readInt
                 , readHex
                 , showIntAtBase
                 )


------------------------------------------------------------------------
-- | A wrapper around 'Word32' and 'Word64' types
data WordF = WordF32 Word32
           | WordF64 Word64
           deriving (Eq, Show)


------------------------------------------------------------------------
-- Generic truncation functions

truncateWord :: WordF -> Int -> WordF
truncateWord (WordF32 b) n = let lastBit = 23 - n in WordF32 (truncateWord' b lastBit)
truncateWord (WordF64 b) n = let lastBit = 52 - n in WordF64 (truncateWord' b lastBit)

truncateWord' :: (Num a, Bits a) => a -> Int -> a
truncateWord' bits lastBit
    | lastBit <= 0                  = bits
    | requiresRounding bits lastBit = dropBits lastBit (bits + 2 ^ lastBit)
    | otherwise                     = dropBits lastBit bits

requiresRounding :: (Bits a) => a -> Int -> Bool
requiresRounding bits lastBit
  | isHalfway = testBit bits lastBit
  | otherwise = testBit bits (lastBit - 1)
  where
    isHalfway = testBit bits (lastBit - 1) && allZeros
    allZeros  = all (== False) (map (testBit bits) checkBits)
    checkBits = filter (\ x -> x < lastBit - 1) [0.. lastBit-2]

dropBits :: (Bits a) => Int -> a -> a
dropBits n = (flip shiftL) n . (flip shiftR) n


-----------------------------------------------------------------------
-- Functions for handling binary representations

binToWord :: Num a => String -> a
binToWord b = fst $ head $ readInt 2 (`elem` "01") digitToInt b

wordToBin :: (Show a, Integral a) => a -> String
wordToBin b = showIntAtBase 2 intToDigit b ""

validBin :: String -> Bool
validBin = all (`elem` "01")

-----------------------------------------------------------------------
-- Functions for handling hexadecimal representations

hexToWord :: (Num a, Eq a) => String -> a
hexToWord s = fst (head (readHex s))

wordToHex :: (Show a, Integral a) => a -> String
wordToHex b = showIntAtBase 16 intToDigit b ""

validHex :: String -> Bool
validHex = all (`elem` "0123456789abcdef")


------------------------------------------------------------------------
-- Padding/stripping leading zeros

zeroPad :: Int -> String -> String
zeroPad n bs
  | length bs < n = replicate (n - length bs) '0' ++ bs
  | otherwise = bs

zeroStrip :: String -> String
zeroStrip = dropWhile (== '0')


------------------------------------------------------------------------
-- generating error messages for 'TParser' functions

tooManyDigits :: String -> Int -> String
tooManyDigits name size = errmsg name size "too many digits"

invalidDigits :: String -> Int -> String
invalidDigits name size = errmsg name size "invalid digits"

errmsg :: String -> Int -> String -> String
errmsg name size reason = mconcat [ "Error: value is not a "
                                  , (show size)
                                  , "-bit "
                                  , name
                                  , " number ("
                                  , reason
                                  , ")"
                                  ]
