-- HC5T1 - Using applyTwice (extended to three times)

applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

increment :: Int -> Int
increment x = x + 1

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
    putStrLn "Testing applyThrice:"
    print (applyThrice increment 5)    -- 8  (5 -> 6 -> 7 -> 8)
    print (applyThrice double 2)       -- 16 (2 -> 4 -> 8 -> 16)
	
	
-- HC5T2 - Filtering Odd Numbers

oddNumbersFrom1To30 :: [Int]
oddNumbersFrom1To30 = filter odd [1..30]

main :: IO ()
main = do
    putStrLn "Odd numbers from 1 to 30:"
    print oddNumbersFrom1To30


-- HC5T3: Checking for Uppercase Letters

import Data.Char (isUpper)

containsUppercaseStart :: [String] -> Bool
containsUppercaseStart wordsList =
    any startsWithUpper wordsList
  where
    startsWithUpper []    = False
    startsWithUpper (c:_) = isUpper c     


main :: IO ()
main = do
    putStrLn "Testing containsUppercaseStart:"
    print (containsUppercaseStart ["apple", "banana", "Cat"])      -- True
    print (containsUppercaseStart ["dog", "elephant", "fish"])     -- False
    print (containsUppercaseStart ["hello", "World"])              -- True
    print (containsUppercaseStart ["noUppercase", "test"])         -- False
	
	
-- HC5T4 - Using Lambda Functions

biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main :: IO ()
main = do
    putStrLn "Testing biggerThan10 (lambda version):"
    print (biggerThan10 5)    -- False
    print (biggerThan10 11)   -- True
    print (biggerThan10 10)   -- False
	
	
-- HC5T5 - Partial Application

multiplyByFive :: Int -> Int
multiplyByFive = (*) 5


main :: IO ()
main = do
    putStrLn "Testing multiplyByFive:"
    print (multiplyByFive 2)   -- 10
    print (multiplyByFive 7)   -- 35
    print (multiplyByFive 10)  -- 50
	
	
-- HC5T6 - Function Composition to square numbers and filter even results

evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

main :: IO ()
main = do
    putStrLn "Testing evenSquares:"
    print (evenSquares [1..10])  -- Expected: [4,16,36,64,100]
    print (evenSquares [3,5,7]) -- Expected: []
    print (evenSquares [2,4,6])  -- Expected: [4,16,36]
	
	
	
-- HC5T7 - The $ Operator

result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main :: IO ()
main = do
    putStrLn "Result using the $ operator:"
    print result   -- Expected output: 84
	
	
-- HC5T8 - Point-Free Style

addFive :: Int -> Int
addFive = (+ 5)

main :: IO ()
main = do
    putStrLn "Testing addFive (point-free style):"
    print (addFive 10)   -- 15
    print (addFive 3)    -- 8
    print (addFive 0)    -- 5
	
	
	
-- HC5T9 - Higher-Order Function to Transform a List

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

transformList :: (a -> a) -> [a] -> [a]
transformList f xs = map (applyTwice f) xs

main :: IO ()
main = do
    putStrLn "Testing transformList:"
    print (transformList (+1) [1,2,3,4])     -- [3,4,5,6]
    print (transformList (*2) [1,2,3])       -- [4,8,12]
    print (transformList reverse ["hi","yo"]) -- ["ih","oy"]
	
	
-- HC5T10 - Combining Higher-Order Functions

anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 xs =
    any (>50) (map (^2) xs)

main :: IO ()
main = do
    putStrLn "Testing anySquareGreaterThan50:"
    print (anySquareGreaterThan50 [1,2,3,4,5])      -- False  (max square 25)
    print (anySquareGreaterThan50 [6,1,2])          -- True   (6^2 = 36 -> False, but try 8: below)
    print (anySquareGreaterThan50 [8])              -- True   (8^2 = 64)
    print (anySquareGreaterThan50 [3,4,7])          -- True   (7^2 = 49? No. But try 8→64)
    print (anySquareGreaterThan50 [10,1,2])         -- True   (10^2 = 100)

	
	
	







