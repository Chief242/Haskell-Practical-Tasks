-- HC6T1 - Factorial (Recursive)

factorial :: Integer -> Integer
factorial 0 = 1                           
factorial n = n * factorial (n - 1)       

main :: IO ()
main = do
    putStrLn "Testing factorial:"
    print (factorial 0)   -- 1
    print (factorial 1)   -- 1
    print (factorial 5)   -- 120
    print (factorial 10)  -- 3628800
	
	
-- HC6T2 - Fibonacci (Recursive)

fibonacci :: Integer -> Integer
fibonacci 0 = 0                     
fibonacci 1 = 1                      
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)   

main :: IO ()
main = do
    putStrLn "Testing fibonacci:"
    print (fibonacci 0)    -- 0
    print (fibonacci 1)    -- 1
    print (fibonacci 5)    -- 5
    print (fibonacci 10)   -- 55
    print (fibonacci 12)   -- 144
	
	
-- HC6T3 - Sum of Elements Using foldr

sumList :: [Int] -> Int
sumList = foldr (+) 0

main :: IO ()
main = do
    putStrLn "Testing sumList using foldr:"
    print (sumList [])            -- 0
    print (sumList [1,2,3,4,5])   -- 15
    print (sumList [10,20,30])    -- 60
    print (sumList [100])         -- 100



-- HC6T4 - Product of Elements Using foldl

productList :: [Int] -> Int
productList = foldl (*) 1


main :: IO ()
main = do
    putStrLn "Testing productList using foldl:"
    print (productList [])            -- 1
    print (productList [1,2,3,4,5])   -- 120
    print (productList [10,2,3])      -- 60
    print (productList [7])           -- 7
	
	
-- HC6T5 - Reverse a List (Recursive)

reverseList :: [a] -> [a]
reverseList []     = []                 
reverseList (x:xs) = reverseList xs ++ [x]  


main :: IO ()
main = do
    putStrLn "Testing reverseList:"
    print (reverseList ([] :: [Int]))      -- []
    print (reverseList [1,2,3,4,5])        -- [5,4,3,2,1]
    print (reverseList "hello")            -- "olleh"
    print (reverseList ['a','b','c'])      -- "cba"
	
	

-- HC6T6 - Element Exists in List

elementExists :: (Eq a) => a -> [a] -> Bool
elementExists _ [] = False                     
elementExists x (y:ys)
    | x == y    = True                         
    | otherwise = elementExists x ys           


main :: IO ()
main = do
    putStrLn "Testing elementExists:"
    print (elementExists 3 [1,2,3,4,5])       -- True
    print (elementExists 10 [1,2,3,4,5])      -- False
    print (elementExists 'a' "haskell")       -- True
    print (elementExists 'z' "haskell")       -- False



-- HC6T7 - List Length

listLength :: [a] -> Int
listLength []     = 0                    
listLength (_:xs) = 1 + listLength xs    


main :: IO ()
main = do
    putStrLn "Testing listLength:"
    print (listLength ([] :: [Int]))     -- 0
    print (listLength [1,2,3,4,5])       -- 5
    print (listLength "hello")           -- 5
    print (listLength ['a','b','c'])     -- 3


-- HC6T8 - Filter Even Numbers

filterEvens :: [Int] -> [Int]
filterEvens xs = filter even xs


main :: IO ()
main = do
    putStrLn "Testing filterEvens:"
    print (filterEvens [])             -- []
    print (filterEvens [1,2,3,4,5,6])  -- [2,4,6]
    print (filterEvens [10,11,12,13])  -- [10,12]
    print (filterEvens [7,9,13])       -- []
	
	
	
-- HC6T9 - Map Implementation

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []                 
myMap f (x:xs) = f x : myMap f xs   


main :: IO ()
main = do
    putStrLn "Testing myMap:"
    print (myMap (+1) [1,2,3,4])         -- [2,3,4,5]
    print (myMap (*2) [5,6,7])           -- [10,12,14]
    print (myMap length ["hi","hello"])  -- [2,5]
    print (myMap reverse ["ab","xyz"])   -- ["ba","zyx"]
	
	
	
-- HC6T10 - Digits of a Number (Recursive)

digits :: Int -> [Int]
digits n
    | n < 10    = [n]                      
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]


main :: IO ()
main = do
    putStrLn "Testing digits:"
    print (digits 0)       -- [0]
    print (digits 7)       -- [7]
    print (digits 123)     -- [1,2,3]
    print (digits 98765)   -- [9,8,7,6,5]






