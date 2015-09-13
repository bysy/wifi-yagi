-- Author: Benjamin Schulz
-- License: http://creativecommons.org/licenses/by-nc-sa/4.0/

module Util where

import Data.Foldable (foldl')
import Text.ParserCombinators.Parsec


-- * Parsec

syFile = sepEndBy sline eol

line = sepEndBy word (char ' ')

sline = do { string "SY"; space;
             ident <- many1 alphaNum; char '='; value <- realNumberP;
             return (ident,value) }

number = many1 digit <|>
         do { sign <- (string "-"); n <- many1 digit; return (sign++n) }

-- A promoting match for a real number. It promotes natural numbers
-- (i.e. whole numbers without a point) to real numbers with a point
realNumberP = do
  prePoint <- number
  postPoint <- do { char '.'; n <- number; return n } <|> do { return "0" }
  radix <- do { char 'e'; n <- number; return n } <|> do { return "1" }
  preRadix <- do { return (prePoint++"."++postPoint) }
  ret <- do { return (if radix=="1" then preRadix else preRadix++"e"++radix) }
  return ret

word :: Parser String
word = many1 letter <?> "word"
eol :: Parser String
eol = string "\n"

-- * Utilities

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)

infixl 0 |>
(|>) :: b -> (b -> b1) -> b1
(|>) = flip ($)

fmod x y = let p = snd <| properFraction (x / y) in p * y
foldlc' :: Num i => 
        (i -> a -> b -> a)   -- ^ Accumulator function
         -> i                -- ^ Initial count
         -> a                -- ^ Initial state
         -> [b]              -- ^ List to process
         -> a                -- ^ Final state
foldlc' f i0 state0 xs = foldl' f' (i0, state0) xs |> snd
    where f' (i, state) x = (i + 1, f i state x)

