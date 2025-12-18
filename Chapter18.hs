-- HC18T1 - mapToLower Function with fmap
-- Converts all characters in a string to lowercase using fmap.

module Main where

import Data.Char (toLower)

-- Function to convert all characters to lowercase using fmap
mapToLower :: String -> String
mapToLower = fmap toLower

-- Main function to test mapToLower
main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    putStrLn ("Lowercase string: " ++ mapToLower input)



-- HC18T2 - Functor Instance for Tree
-- Create a Functor instance for a binary tree.

module Main where

-- Binary tree data type
data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Functor instance for Tree
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node value left right) =
        Node (f value) (fmap f left) (fmap f right)

-- Example tree
exampleTree :: Tree Int
exampleTree =
    Node 10
        (Node 5 Empty Empty)
        (Node 20
            (Node 15 Empty Empty)
            Empty
        )

-- Main function to demonstrate fmap on Tree
main :: IO ()
main = do
    putStrLn "Original tree:"
    print exampleTree

    putStrLn "\nTree after applying fmap (*2):"
    print (fmap (*2) exampleTree)

    putStrLn "\nTree after applying fmap show:"
    print (fmap show exampleTree)



-- HC18T3 - incrementTreeValues Function
-- Adds 1 to every value in a tree using the Functor instance.

module Main where

-- Binary tree data type
data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Functor instance for Tree
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node value left right) =
        Node (f value) (fmap f left) (fmap f right)

-- Function to increment every value in the tree
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

-- Example tree
exampleTree :: Tree Int
exampleTree =
    Node 1
        (Node 2 Empty Empty)
        (Node 3
            (Node 4 Empty Empty)
            Empty
        )

-- Main function to demonstrate incrementTreeValues
main :: IO ()
main = do
    putStrLn "Original tree:"
    print exampleTree

    putStrLn "\nTree after incrementing all values:"
    print (incrementTreeValues exampleTree)


-- HC18T4 - mapToBits Function
-- Converts a list of Booleans to '1' or '0' using fmap.

module Main where

-- Function to map Booleans to bits using fmap
mapToBits :: [Bool] -> String
mapToBits = fmap boolToBit
  where
    boolToBit :: Bool -> Char
    boolToBit True  = '1'
    boolToBit False = '0'

-- Main function to demonstrate mapToBits
main :: IO ()
main = do
    let values = [True, False, True, True, False]

    putStrLn "Original Boolean list:"
    print values

    putStrLn "Converted to bits:"
    print (mapToBits values)



-- HC18T5 - Functor Instance for Either
-- fmap applies only to the Right value

module Main where

-- Custom Either type (to show the Functor instance explicitly)
data MyEither a b
    = MyLeft a
    | MyRight b
    deriving (Show, Eq)

-- Functor instance
-- Only applies the function to MyRight
instance Functor (MyEither a) where
    fmap _ (MyLeft x)  = MyLeft x
    fmap f (MyRight y) = MyRight (f y)

-- Main function to demonstrate fmap behavior
main :: IO ()
main = do
    let success = MyRight 10
    let failure = MyLeft "Error occurred"

    putStrLn "Applying fmap (*2) to MyRight:"
    print (fmap (*2) success)

    putStrLn "Applying fmap (*2) to MyLeft:"
    print (fmap (*2) failure)

    putStrLn "\nApplying fmap show to MyRight:"
    print (fmap show success)



-- HC18T6 - applyToMaybe Function
-- Uses fmap to transform the value inside a Maybe.

module Main where

-- Function that applies a transformation to a Maybe value using fmap
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

-- Main function to demonstrate applyToMaybe
main :: IO ()
main = do
    let value1 = Just 10
    let value2 = Nothing

    putStrLn "Applying (*2) to Just 10:"
    print (applyToMaybe (*2) value1)

    putStrLn "Applying (*2) to Nothing:"
    print (applyToMaybe (*2) value2)

    putStrLn "\nApplying show to Just 42:"
    print (applyToMaybe show (Just 42))



-- HC18T7 - fmapTuple Function
-- Applies a function to the second element of a tuple using Functor.

module Main where

-- Function that applies a function to the second element of a tuple
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

-- Main function to demonstrate fmapTuple
main :: IO ()
main = do
    let tuple1 = ("age", 21)
    let tuple2 = ("count", 10)

    putStrLn "Original tuples:"
    print tuple1
    print tuple2

    putStrLn "\nApplying fmapTuple (+1):"
    print (fmapTuple (+1) tuple1)
    print (fmapTuple (+1) tuple2)

    putStrLn "\nApplying fmapTuple show:"
    print (fmapTuple show tuple1)



-- HC18T8 - identityLawCheck Function
-- Verifies the Functor identity law: fmap id x == x

module Main where

-- Function to check the Functor identity law
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

-- Main function to demonstrate identityLawCheck
main :: IO ()
main = do
    putStrLn "Checking Functor identity law:\n"

    putStrLn "Maybe examples:"
    print (identityLawCheck (Just 10))     -- True
    print (identityLawCheck Nothing)       -- True

    putStrLn "\nList examples:"
    print (identityLawCheck [1,2,3])       -- True
    print (identityLawCheck ([] :: [Int])) -- True

    putStrLn "\nTuple example:"
    print (identityLawCheck ("count", 5))  -- True

    putStrLn "\nEither example:"
    print (identityLawCheck (Right 42 :: Either String Int)) -- True
    print (identityLawCheck (Left "error" :: Either String Int)) -- True



-- HC18T9 - compositionLawCheck Function
-- Verifies the Functor composition law:
-- fmap (f . g) x == fmap f (fmap g x)

module Main where

-- Function to check the Functor composition law
compositionLawCheck
    :: (Functor f, Eq (f c))
    => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x =
    fmap (f . g) x == (fmap f . fmap g) x

-- Main function to demonstrate compositionLawCheck
main :: IO ()
main = do
    putStrLn "Checking Functor composition law:\n"

    let f = (*2)
    let g = (+1)

    putStrLn "Maybe examples:"
    print (compositionLawCheck f g (Just 10))   -- True
    print (compositionLawCheck f g Nothing)     -- True

    putStrLn "\nList examples:"
    print (compositionLawCheck f g [1,2,3])     -- True
    print (compositionLawCheck f g ([] :: [Int])) -- True

    putStrLn "\nTuple example:"
    print (compositionLawCheck f g ("count", 5)) -- True

    putStrLn "\nEither examples:"
    print (compositionLawCheck f g (Right 7 :: Either String Int)) -- True
    print (compositionLawCheck f g (Left "error" :: Either String Int)) -- True



-- HC18T10 - nestedFmap Function
-- Applies a function to a nested Functor structure using multiple fmap calls.

module Main where

-- Function that applies a function to a nested structure (Maybe [a])
nestedFmap :: (a -> b) -> Maybe [a] -> Maybe [b]
nestedFmap f = fmap (fmap f)
-- Equivalent to: nestedFmap f x = fmap (map f) x

-- Main function to demonstrate nestedFmap
main :: IO ()
main = do
    let value1 = Just [1, 2, 3]
    let value2 = Nothing :: Maybe [Int]

    putStrLn "Applying nestedFmap (*2) to Just [1,2,3]:"
    print (nestedFmap (*2) value1)

    putStrLn "\nApplying nestedFmap (*2) to Nothing:"
    print (nestedFmap (*2) value2)

    putStrLn "\nApplying nestedFmap show to Just [4,5,6]:"
    print (nestedFmap show (Just [4,5,6]))
