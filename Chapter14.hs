--HC14T1: Initialize a Cabal Project
--app/Main.hs


module Main where

main :: IO ()
main = putStrLn "Hello, Cabal!"



--HC14T2: Add Dependency and Print Random Number
--app/Main.hs

module Main where

import System.Random (randomRIO)

main :: IO ()
main = do
    num <- randomRIO (1, 100)  -- generate a random number between 1 and 100
    putStrLn ("Your random number is: " ++ show num)


--HC14T3: NumericUnderscores Extension
--app/Main.hs

{-# LANGUAGE NumericUnderscores #-}

module Main where

main :: IO ()
main = do
    let bigNumber1 = 1_000_000
    let bigNumber2 = 2_500_000_000
    let bigNumber3 = 123_456_789_012_345

    putStrLn "Printing numbers with NumericUnderscores enabled:"
    print bigNumber1
    print bigNumber2
    print bigNumber3


--HC14T4: TypeApplications Extension
--app/Main.hs

{-# LANGUAGE TypeApplications #-}

module Main where

-- Function that converts a String to an Int using TypeApplications
stringToInt :: String -> Int
stringToInt s = read @Int s

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    
    let n = stringToInt input
    putStrLn ("Converted integer: " ++ show n)
	
	read (input) :: Int
	read @Int input

stringToInt :: String -> Int
stringToInt s = read @Int s


--HC14T5: Custom Data Type and Pattern Matching with @

{-# LANGUAGE TypeApplications #-}

module Main where

-- Custom data type
data Result a
    = Ok a
    | Error String
    deriving Show

-- Function demonstrating pattern matching with @
describeResult :: Result Int -> String
describeResult full@(Ok value) =
    "Pattern matched Ok using @: full value = " ++ show full
    ++ ", extracted value = " ++ show value

describeResult full@(Error msg) =
    "Pattern matched Error using @: full value = " ++ show full
    ++ ", message = " ++ msg

main :: IO ()
main = do
    let r1 = Ok 42
    let r2 = Error "Something went wrong"

    putStrLn (describeResult r1)
    putStrLn (describeResult r2)


--HC14T6: Project Structure: src and app
-- Create the file: src/MyModule.hs

module MyModule (greet) where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

 -- Modify app/Main.hs

module Main where

import MyModule  -- from src/

main :: IO ()
main = do
    putStrLn (greet "Cabal Project")


--HC14T7: Library Component in Cabal
--src/MyModule.hs

module MyModule (greet) where

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

--app/Main.hs

module Main where

import MyModule

main :: IO ()
main = putStrLn (greet "from the library")


--HC14T8: Character Frequency Function

module Main where

import Data.List (sort, group)

-- | counts: returns a list of (character, frequency) pairs
counts :: String -> [(Char, Int)]
counts str =
    let grouped = group (sort str)        -- group identical chars together
    in [ (head g, length g) | g <- grouped ]

main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine

    putStrLn "Character frequencies:"
    print (counts input)


--HC14T9: PartialTypeSignatures Extension

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Text.Read (readMaybe)

{-|
  Demonstrates PartialTypeSignatures.

  We write a function `parseInt` whose signature uses a wildcard `_`.
  The wildcard lets GHC infer the exact type, but the source still shows a placeholder.

  We suppress the compiler warning about partial type signatures with
  the OPTIONS_GHC pragma above so the example compiles without extra noise.
-}

-- Wildcard in the result type. GHC will infer it (Int in this example).
parseInt :: String -> _
parseInt s =
  case (readMaybe s :: Maybe Int) of
    Just n  -> n
    Nothing -> 0

-- Another example: wildcard in an argument position.
-- The function accepts some value (inferred to be Int here) and returns its double.
doubleValue :: _ -> Int
doubleValue x = (x :: Int) * 2

main :: IO ()
main = do
    putStrLn "Enter a number (for parseInt):"
    line <- getLine
    let n = parseInt line
    putStrLn $ "parseInt produced: " ++ show n

    let d = doubleValue n
    putStrLn $ "doubleValue applied to that: " ++ show d


--HC14T10: Cabal Test Suite

module Counts (counts) where

import Data.List (sort, group)

-- counts: returns a list of (character, frequency) pairs
counts :: String -> [(Char, Int)]
counts str =
    let grouped = group (sort str)
    in [ (head g, length g) | g <- grouped ]



module Main (main) where

import Test.Hspec
import Counts (counts)

main :: IO ()
main = hspec $ do
  describe "Counts.counts" $ do
    it "counts characters in \"banana\"" $ do
      counts "banana" `shouldBe` [('a',3),('b',1),('n',2)]

    it "returns [] for empty string" $ do
      counts "" `shouldBe` []

    it "counts repeated characters correctly" $ do
      counts "aabbcc" `shouldBe` [('a',2),('b',2),('c',2)]

    it "is insensitive to order of input (result sorted by character)" $ do
      counts "cab" `shouldBe` [('a',1),('b',1),('c',1)]

