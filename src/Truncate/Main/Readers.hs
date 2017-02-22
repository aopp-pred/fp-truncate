{- |
Description : Readers for parsing inputs with optparse-applicative
Copyright   : 2017 Andrew Dawson
License     : Apache-2.0

Readers for validating strings as binary, decimal and hexadecimal. These are
used when parsing command line arguments allowing a program using the
optparse-applicative library to fail gracefully on bad input.
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
module Truncate.Main.Readers
    ( binary
    , decimal
    , hexadecimal
    ) where


------------------------------------------------------------------------
import Text.Read           (readMaybe)
import Options.Applicative (eitherReader, ReadM)


------------------------------------------------------------------------

-- | Read a binary string (@0@s and @1@s) or fail to parse.
binary :: ReadM String
binary = eitherReader bsReader
  where
    bsReader s
      | all (`elem` "01") s = Right s
      | otherwise           = Left "Error: value is not a binary number"

-- | Read a decimal string or fail to parse.
decimal :: ReadM String
decimal = eitherReader dsReader
  where
    dsReader s = case readMaybe s :: Maybe Double of
                     Just _  -> Right s
                     Nothing -> Left "Error: value is not a decimal number"

-- | Read a hexadecimal string or fail to parse.
hexadecimal :: ReadM String
hexadecimal = eitherReader hsReader
  where
    hex = "0123456789abcdefABCDEF"
    hsReader s
      | all (`elem` hex) s = Right s
      | otherwise          = Left "Error: value is not a hexadecimal number"
