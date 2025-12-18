-- HC12T1 - Print a Welcome Message
-- This program prints a welcome message to the terminal.

main :: IO ()
main = do
    putStrLn "Welcome to Haskell Programming!"



-- HC12T2 - Add Two Numbers
-- Define a function addTwoNumbers and print the result in main.

-- Function definition
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- Main program to print the result
main :: IO ()
main = do
    putStrLn "Enter the first number:"
    n1 <- getLine

    putStrLn "Enter the second number:"
    n2 <- getLine

    let a = read n1 :: Int
    let b = read n2 :: Int
    let result = addTwoNumbers a b

    putStrLn ("The sum is: " ++ show result)




-- HC12T3 - Factorial Function
-- Define a function factorial to compute the factorial of a positive integer.

-- Recursive factorial function
factorial :: Int -> Int
factorial 0 = 1                    -- base case
factorial n = n * factorial (n-1)  -- recursive case

-- Main program to test the factorial function
main :: IO ()
main = do
    putStrLn "Enter a positive integer:"
    input <- getLine
    let n = read input :: Int
    putStrLn ("Factorial: " ++ show (factorial n))



-- HC12T4 - First 10 Fibonacci Numbers
-- Calculate and print the first 10 Fibonacci numbers using recursion.

-- Recursive Fibonacci function
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Generate a list of the first 10 Fibonacci numbers
first10Fibs :: [Int]
first10Fibs = [ fib n | n <- [0..9] ]

-- Main program
main :: IO ()
main = do
    putStrLn "The first 10 Fibonacci numbers are:"
    print first10Fibs



-- HC12T5 - Palindrome Checker
-- Define a function isPalindrome that checks if a string is a palindrome.

-- Palindrome function
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- Main program to test with user input
main :: IO ()
main = do
    putStrLn "Enter a string to check if it's a palindrome:"
    input <- getLine

    if isPalindrome input
        then putStrLn "Yes! It is a palindrome."
        else putStrLn "No, it is not a palindrome."



-- HC12T6 - Sort a List of Integers
-- Read a list of integers from the user and print it sorted.

import Data.List (sort)

main :: IO ()
main = do
    putStrLn "Enter a list of integers separated by spaces:"
    input <- getLine

    -- Split the input into words, convert each to Int
    let numbers = map read (words input) :: [Int]

    -- Sort the list
    let sortedList = sort numbers

    putStrLn ("Sorted list: " ++ show sortedList)




-- HC12T7 - Calculate Circle Area
-- Define a function calculateCircleArea and demonstrate its usage in main.

-- Function to calculate the area of a circle
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

main :: IO ()
main = do
    putStrLn "Enter the radius of the circle:"
    input <- getLine

    let radius = read input :: Float
    let area = calculateCircleArea radius

    putStrLn ("The area of the circle is: " ++ show area)




-- HC12T8 - Merge Two Sorted Lists
-- Define a function mergeLists that merges two sorted lists.

-- Recursive merge function
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

-- Main program to test the merge functionality
main :: IO ()
main = do
    let list1 = [1, 3, 5, 7]
    let list2 = [2, 4, 6, 8, 10]

    putStrLn "Merging two sorted lists:"
    print (mergeLists list1 list2)



-- HC12T9 - Read and Print File Content
-- Read a file and print its content, handling errors gracefully.

import System.IO
import System.IO.Error (catchIOError)

-- Safely read a file
safeReadFile :: FilePath -> IO ()
safeReadFile path = do
    contentOrError <- readFile path `catchIOError` handler
    putStrLn contentOrError
  where
    handler e = return ("Error: Could not open file.\nDetails: " ++ show e)

main :: IO ()
main = do
    putStrLn "Enter the file path to read:"
    path <- getLine
    safeReadFile path




-- MathOps.hs
-- HC12T10 - Mathematical Operations Module

module MathOps
    ( add
    , subtract'
    , multiply
    , divide
    ) where

-- Add two numbers
add :: Float -> Float -> Float
add x y = x + y

-- Subtract two numbers
subtract' :: Float -> Float -> Float
subtract' x y = x - y

-- Multiply two numbers
multiply :: Float -> Float -> Float
multiply x y = x * y

-- Divide two numbers (simple version)
divide :: Float -> Float -> Float
divide x y = x / y




-- Main.hs
-- Demonstrate usage of the MathOps module

import MathOps

main :: IO ()
main = do
    putStrLn "Demonstrating MathOps module:"

    let a = 10
    let b = 5

    putStrLn ("Add: " ++ show (add a b))
    putStrLn ("Subtract: " ++ show (subtract' a b))
    putStrLn ("Multiply: " ++ show (multiply a b))
    putStrLn ("Divide: " ++ show (divide a b))
