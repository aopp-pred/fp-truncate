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
module Truncate
    ( Truncatable(..)
    , TParser
    , WordF(..)
    , Binary(..)
    , Decimal(..)
    , Hexadecimal(..)
    , toBin
    , toDec
    , toHex
    , makeBin32
    , makeBin64
    , makeDec32
    , makeDec64
    , makeHex32
    , makeHex64
    ) where


------------------------------------------------------------------------
import Data.Binary.IEEE754 ( wordToFloat
                           , floatToWord
                           , wordToDouble
                           , doubleToWord
                           )
import Text.Read           (readMaybe)


------------------------------------------------------------------------
import Truncate.Internal


------------------------------------------------------------------------
-- | A typeclass for things that can be represented as truncatable bits
class Truncatable a where

    -- | Convert to a bits representation, either a 'WordF32' or 'WordF64'.
    makeBits :: a -> WordF
    -- | Convert from a bits representation which will be either a 'WordF32'
    -- or a 'WordF64'.
    fromBits :: WordF -> a

    -- | Truncation operator, truncates an instance to a specified number of
    -- bits in its significand. The truncation applies the IEEE round-to-nearest
    -- tie-to-even rounding specification.
    infixl 5 <#
    (<#) :: a -> Int -> a
    (<#) = (fromBits .) . truncateWord . makeBits
    -- | The same as '(<#)' with the arguments flipped.
    infixr 5 #>
    (#>) :: Int -> a -> a
    (#>)= flip (<#)
    -- | Convert from one 'Truncatable' instance to another.
    convert :: (Truncatable b) => a -> b
    convert = fromBits . makeBits


-----------------------------------------------------------------------
-- | A function type for parsing 'Truncatable' types
type TParser a = String -> Either String a


-----------------------------------------------------------------------
-- | Truncatable floating-point numbers in binary representation.
data Binary = Bin32 String
            | Bin64 String

instance Truncatable Binary where
    makeBits (Bin32 b)   = WordF32 $ binToWord b
    makeBits (Bin64 b)   = WordF64 $ binToWord b
    fromBits (WordF32 b) = Bin32 (wordToBin b)
    fromBits (WordF64 b) = Bin64 (wordToBin b)

instance Show Binary where
    show (Bin32 s) = zeroPad 32 s
    show (Bin64 s) = zeroPad 64 s

instance Eq Binary where
    (Bin32 a) == (Bin32 b) = (zeroStrip a) == (zeroStrip b)
    (Bin64 a) == (Bin64 b) = (zeroStrip a) == (zeroStrip b)
    _ == _                 = False

-- | Convert a 'Truncatable' instance to 'Binary'.
toBin :: (Truncatable a) => a -> Binary
toBin = convert

-- | Create a 'Bin32' from a string. The string should be a sequence of the
-- characters @0@ and @1@ no longer than 32 characters in length.
makeBin32 :: TParser Binary
makeBin32 s
  | length s > 32     = Left $ tooManyDigits "binary" 32
  | (not .validBin) s = Left $ invalidDigits "binary" 32
  | otherwise         = Right (Bin32 s)

-- | Create a 'Bin64' from a string. The string should be a sequence of the
-- characters @0@ and @1@ no longer than 64 characters in length.
makeBin64 :: TParser Binary
makeBin64 s
  | length s > 64     = Left $ tooManyDigits "binary" 64
  | (not .validBin) s = Left $ invalidDigits "binary" 64
  | otherwise = Right (Bin64 s)


-----------------------------------------------------------------------
-- | Truncatable floating-point numbers in decimal representation.
data Decimal = Dec32 Float
             | Dec64 Double
             deriving (Eq)

instance Truncatable Decimal where
    makeBits (Dec32 f)   = WordF32 (floatToWord f)
    makeBits (Dec64 f)   = WordF64 (doubleToWord f)
    fromBits (WordF32 b) = Dec32 (wordToFloat b)
    fromBits (WordF64 b) = Dec64 (wordToDouble b)

instance Show Decimal where
    show (Dec32 f) = show f
    show (Dec64 d) = show d

-- | Convert a 'Truncatable' instance to 'Decimal'.
toDec :: (Truncatable a) => a -> Decimal
toDec = convert

-- | Create a 'Dec32' from a string. The string can be anything that can be
-- interpreted as a 32-bit 'Float' by 'read'.
makeDec32 :: TParser Decimal
makeDec32 s = case readMaybe s :: Maybe Float of
    Just f  -> Right (Dec32 f)
    Nothing -> Left "Error: value is not a 32-bit decimal number"

-- | Create a 'Dec64' from a string. The string can be anything that can be
-- interpreted as a 64-bit 'Double' by 'read'.
makeDec64 :: TParser Decimal
makeDec64 s = case readMaybe s :: Maybe Double of
    Just f  -> Right (Dec64 f)
    Nothing -> Left  "Error: value is not a 64-bit decimal number"


-----------------------------------------------------------------------
-- | Truncatable floating-point numbers in hexadecimal representation.
data Hexadecimal = Hex32 String
                 | Hex64 String

instance Truncatable Hexadecimal where
    makeBits (Hex32 s)   = WordF32 $ hexToWord s
    makeBits (Hex64 s)   = WordF64 $ hexToWord s
    fromBits (WordF32 b) = Hex32 (wordToHex b)
    fromBits (WordF64 b) = Hex64 (wordToHex b)

instance Show Hexadecimal where
    show (Hex32 s) = zeroPad 8 s
    show (Hex64 s) = zeroPad 16 s

instance Eq Hexadecimal where
    (Hex32 a) == (Hex32 b) = (zeroStrip a) == (zeroStrip b)
    (Hex64 a) == (Hex64 b) = (zeroStrip a) == (zeroStrip b)
    _ == _                 = False

-- | Convert a 'Truncatable' instance to 'Hexadecimal'.
toHex :: (Truncatable a) => a -> Hexadecimal
toHex = convert

-- | Create a 'Hex32' from a string. The string should be a sequence of the
-- valid hexadecimal digits @0-9@ and @a-f@ no longer than 8 characters in length.
makeHex32 :: TParser Hexadecimal
makeHex32 s
  | length s > 8       = Left $ tooManyDigits "hexadecimal" 32
  | (not . validHex) s = Left $ invalidDigits "hexadecimal" 32
  | otherwise          = Right (Hex32 s)

-- | Create a 'Hex64' from a string. The string should be a sequence of the
-- valid hexadecimal digits @0-9@ and @a-f@ no longer than 16 characters in length.
makeHex64 :: TParser Hexadecimal
makeHex64 s
  | length s > 16      = Left $ tooManyDigits "hexadecimal" 64
  | (not . validHex) s = Left $ invalidDigits "hexadecimal" 64
  | otherwise          = Right (Hex64 s)
