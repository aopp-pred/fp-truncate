{- |
Description : Main program for truncating floating-point significands
Copyright   : 2017 Andrew Dawson
License     : Apache-2.0

A main program function for applying the truncation tools provided by
'Truncate' to command line inputs.
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
module Truncate.Main
    ( btMain
    ) where


------------------------------------------------------------------------
import System.Environment  (getProgName)
import System.Exit         ( ExitCode(..)
                           , exitWith
                           )


------------------------------------------------------------------------
import Data.Monoid         ((<>))
import Options.Applicative


------------------------------------------------------------------------
import Truncate            ( Truncatable(..)
                           , toBin
                           , toDec
                           , toHex
                           )


------------------------------------------------------------------------
-- | A main function for implementing programs that operate on 'Truncatable'
-- instances. This function will apply specified truncations to the input
-- and print the output to stdout. This function sets the exit code to @0@
-- on success, and @>0@ otherwise.
btMain :: (Show b, Truncatable b)
    => ReadM String   -- ^ A reader for the type of input required by the
                      -- program for validating (but not parsing) an input value.
                      -- The 'Tuncate.Main.Readers' module provides readers for
                      -- binary, decimal and hexadecimal inputs.
    -> (String -> b)  -- ^ A parser to generate a 'Truncatable' instance from
                      -- a string (e.g., 'makeBin32').
    -> String         -- ^ A short (one line) description of program's purpose.
    -> IO ()
btMain reader parser description =
    btMainWithExitCode reader parser description >>= exitWith

btMainWithExitCode :: (Show b, Truncatable b) =>
    ReadM String -> (String -> b) -> String -> IO ExitCode
btMainWithExitCode reader parser description = do
    prog <- getProgName
    let optionParser  = makeParser reader parser
        programParser = info (helper <*> optionParser)
                             (header $ prog ++ " - " ++ description)
    options <- execParser programParser
    runProgram options

runProgram :: (Show a, Truncatable a) => Options a -> IO ExitCode
runProgram (Options val bs b)
  | b == 0    = mapM_ print truncated >> return ExitSuccess
  | b == 2    = mapM_ print (map toBin truncated) >> return ExitSuccess
  | b == 10   = mapM_ print (map toDec truncated) >> return ExitSuccess
  | b == 16   = mapM_ print (map toHex truncated) >> return ExitSuccess
  | otherwise = putStrLn ("error: invalid base " ++ show b) >> return (ExitFailure 1)
  where
    truncated         = map (val <#) (makeRange bs)
    makeRange []      = []
    makeRange [n]     = [n]
    makeRange (n:m:_) = if m > n then [n..m] else reverse [m..n]


------------------------------------------------------------------------

data Options a = Options { inputValue  :: a
                         , outputBits  :: [Int]
                         , outputBase  :: Int
                         }
               deriving (Show)

makeParser :: ReadM a -> (a -> b) -> Parser (Options b)
makeParser reader parser = optionParser
  where
    optionParser = Options <$> valueParser <*> bitsParser <*> baseParser
    valueParser  = parser <$> (argument reader (metavar "value"))
    bitsParser   = or12 $ argument auto (metavar "bits")
    baseParser   = option auto (  short 'b'
                               <> long "base"
                               <> help "base"
                               <> value 0
                               <> metavar "base")

or12 :: (Alternative f) => f a -> f [a]
or12 x = pure (:) <*> x <*> or01 x
  where
    or01 x' = one x' <|> pure []
    one  x' = pure (:) <*> x' <*> pure []
