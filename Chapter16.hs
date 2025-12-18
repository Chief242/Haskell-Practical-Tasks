-- HC16T1 - Reverse a String
-- Define a function to reverse a string and demonstrate its usage.

reverseString :: String -> String
reverseString = reverse

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a string to reverse:"
    input <- getLine
    putStrLn ("Reversed string: " ++ reverseString input)
	
	
-- HC16T2 - Palindrome Checker
-- Define a function that checks if a string is a palindrome.

-- Function to check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- Main function to test the palindrome checker
main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine

    if isPalindrome input
        then putStrLn "The string is a palindrome."
        else putStrLn "The string is not a palindrome."




-- HC16T3 - Factorial
-- Define a function to calculate the factorial of a number.

-- Factorial function (recursive)
factorial :: Integer -> Integer
factorial 0 = 1                 -- base case
factorial n = n * factorial (n - 1)

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a non-negative integer:"
    input <- getLine
    let n = read input :: Integer
    putStrLn ("Factorial: " ++ show (factorial n))



-- HC16T4 - Filter Even Numbers
-- Implement a function that filters only even numbers from a list.

-- Function to filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens xs = filter even xs

-- Main function to test filterEvens
main :: IO ()
main = do
    let numbers = [1..20]
    putStrLn "Original list:"
    print numbers

    putStrLn "Even numbers only:"
    print (filterEvens numbers)




-- HC16T5 - Uppercase String
-- Define a function to convert all characters in a string to uppercase.

import Data.Char (toUpper)

-- Function to convert a string to uppercase
toUpperString :: String -> String
toUpperString = map toUpper

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    putStrLn ("Uppercase string: " ++ toUpperString input)


-- HC16T6 - nth Fibonacci Number
-- Implement a function that returns the nth Fibonacci number.

-- Fibonacci function (recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0          -- base case
fibonacci 1 = 1          -- base case
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a non-negative integer n:"
    input <- getLine
    let n = read input :: Integer
    putStrLn ("The " ++ show n ++ "th Fibonacci number is: " ++ show (fibonacci n))




-- HC16T7 - Element Existence in List
-- Write a function that checks if an element exists in a list.

-- Function to check if an element exists in a list
elementExists :: Eq a => a -> [a] -> Bool
elementExists x xs = x `elem` xs

-- Main function to test elementExists
main :: IO ()
main = do
    let numbers = [1, 3, 5, 7, 9]

    putStrLn "List:"
    print numbers

    putStrLn "Check if 5 exists:"
    print (elementExists 5 numbers)

    putStrLn "Check if 4 exists:"
    print (elementExists 4 numbers)



-- HC16T8 - Insertion Sort
-- Define a function that sorts a list of integers using insertion sort.

-- Insert an element into a sorted list
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : insert x ys

-- Insertion sort function
insertionSort :: [Int] -> [Int]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Main function to test insertionSort
main :: IO ()
main = do
    let numbers = [5, 2, 9, 1, 5, 6]
    putStrLn "Original list:"
    print numbers

    putStrLn "Sorted list (Insertion Sort):"
    print (insertionSort numbers)




-- HC16T9 - Remove Duplicates from List
-- Implement a function to remove duplicate elements from a list.

import Data.List (nub)

-- Function to remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

-- Main function to test removeDuplicates
main :: IO ()
main = do
    let numbers = [1,2,2,3,4,4,5,1,6,3]
    let wordsList = ["apple", "banana", "apple", "orange", "banana"]

    putStrLn "Original numbers list:"
    print numbers

    putStrLn "Numbers list without duplicates:"
    print (removeDuplicates numbers)

    putStrLn "\nOriginal words list:"
    print wordsList

    putStrLn "Words list without duplicates:"
    print (removeDuplicates wordsList)



-- HC16T10 - Character Frequency in String
-- Define a function that counts the frequency of each character in a string.

import Data.List (sort, group)

-- Function to count character frequency
charFrequency :: String -> [(Char, Int)]
charFrequency str =
    let groupedChars = group (sort str)
    in [(head g, length g) | g <- groupedChars]

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine

    putStrLn "Character frequencies:"
    print (charFrequency input)




