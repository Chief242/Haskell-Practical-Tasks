-- HC20T1 - safeDivide with Maybe Monad

module Main where

-- Safe division using Maybe monad
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Main function
main :: IO ()
main = do
    print (safeDivide 10 2)   -- Just 5.0
    print (safeDivide 10 0)   -- Nothing



-- HC20T2 - sequenceMaybe

module Main where

-- Converts [Maybe a] to Maybe [a]
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

-- Main function
main :: IO ()
main = do
    print (sequenceMaybe [Just 1, Just 2, Just 3])
    print (sequenceMaybe [Just 1, Nothing, Just 3])



-- HC20T3 - Writer Monad Logging Calculator

module Main where

import Control.Monad.Writer

-- Adds two numbers and logs the operation
add :: Int -> Int -> Writer [String] Int
add x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

-- Multiplies two numbers and logs the operation
multiply :: Int -> Int -> Writer [String] Int
multiply x y = writer (x * y, ["Multiplied " ++ show x ++ " and " ++ show y])

-- Main function
main :: IO ()
main = do
    let (result, logMessages) = runWriter $ do
            a <- add 3 4
            b <- multiply a 2
            return b

    putStrLn ("Result: " ++ show result)
    putStrLn "Log:"
    mapM_ putStrLn logMessages



-- HC20T4 - countChars with State Monad

module Main where

import Control.Monad.State

-- Counts occurrences of a character in a string
countChars :: Char -> String -> State Int ()
countChars c str = mapM_ updateCount str
  where
    updateCount x =
        when (x == c) $ modify (+1)

-- Main function
main :: IO ()
main = do
    let text = "haskell is awesome"
        charToCount = 's'
        count = execState (countChars charToCount text) 0

    putStrLn ("Character '" ++ [charToCount] ++ "' appears "
              ++ show count ++ " times.")




-- HC20T5 - Reader Monad for Configurable Greeting

module Main where

import Control.Monad.Reader

-- Configuration type
data Config = Config
    { greetingPrefix :: String
    }

-- Greeting function using Reader
greetUser :: String -> Reader Config String
greetUser name = do
    config <- ask
    return (greetingPrefix config ++ ", " ++ name ++ "!")

-- Main function
main :: IO ()
main = do
    let config = Config "Hello"
    let message = runReader (greetUser "Tshifhiwa") config
    putStrLn message




-- HC20T6 - doubleMonad combining Maybe and List

module Main where

-- Combines Maybe and List using do-notation
-- If Maybe is Nothing → result is []
doubleMonad :: Maybe a -> [b] -> [(a, b)]
doubleMonad ma xs = do
    a <- maybe [] pure ma
    b <- xs
    return (a, b)

-- Main function
main :: IO ()
main = do
    print (doubleMonad (Just 10) [1,2,3])
    print (doubleMonad Nothing [1,2,3])



-- HC20T7 - findFirst with Either Monad

module Main where

-- Finds the first element satisfying a predicate
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No matching element found"
findFirst p (x:xs)
    | p x       = Right x
    | otherwise = findFirst p xs

-- Main function
main :: IO ()
main = do
    print (findFirst even [1,3,5,8,10])
    print (findFirst even [1,3,5])



-- HC20T8 - Simple Parser Monad

module Main where

import Control.Applicative

-- Simple Parser type
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor instance
instance Functor Parser where
    fmap f (Parser p) = Parser $ \s ->
        p s >>= \(a, rest) -> Just (f a, rest)

-- Applicative instance
instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    Parser pf <*> Parser pa = Parser $ \s ->
        pf s >>= \(f, s') ->
        pa s' >>= \(a, s'') ->
        Just (f a, s'')

-- Monad instance
instance Monad Parser where
    Parser pa >>= f = Parser $ \s ->
        pa s >>= \(a, s') ->
        runParser (f a) s'

-- Parse a digit
digit :: Parser Char
digit = Parser $ \s -> case s of
    (x:xs) | x >= '0' && x <= '9' -> Just (x, xs)
    _ -> Nothing

-- Parse two digits and add them
addTwoDigits :: Parser Int
addTwoDigits = do
    d1 <- digit
    d2 <- digit
    return (read [d1] + read [d2])

-- Main function
main :: IO ()
main = do
    print (runParser addTwoDigits "34abc")
    print (runParser addTwoDigits "a4")



-- HC20T9 - replicateMonad with Identity Monad

module Main where

import Data.Functor.Identity

-- Replicates a value n times using Identity monad
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = Identity (replicate n x)

-- Main function
main :: IO ()
main = do
    let Identity result = replicateMonad 5 "Haskell"
    print result




-- HC20T10 - Nested StateT and MaybeT

module Main where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

-- StateT Int inside MaybeT IO
type App = MaybeT (StateT Int IO)

-- Increments state if value is positive
incrementIfPositive :: Int -> App ()
incrementIfPositive x =
    if x > 0
        then lift $ modify (+x)
        else MaybeT (return Nothing)

-- Main function
main :: IO ()
main = do
    let computation = do
            incrementIfPositive 5
            incrementIfPositive 3

    (result, finalState) <- runStateT (runMaybeT computation) 0
    print result
    putStrLn ("Final state: " ++ show finalState)




-- HC20T11 - randomWalk with State Monad

module Main where

import Control.Monad.State
import System.Random (randomRIO)

type Position = (Int, Int)

-- Perform one random step
randomStep :: StateT Position IO ()
randomStep = do
    (x, y) <- get
    dir <- liftIO (randomRIO (0, 3) :: IO Int)
    let newPos = case dir of
            0 -> (x + 1, y) -- right
            1 -> (x - 1, y) -- left
            2 -> (x, y + 1) -- up
            _ -> (x, y - 1) -- down
    put newPos

-- Perform n random steps
randomWalk :: Int -> StateT Position IO ()
randomWalk n = replicateM_ n randomStep

-- Main function
main :: IO ()
main = do
    (_, finalPos) <- runStateT (randomWalk 10) (0, 0)
    putStrLn ("Final position: " ++ show finalPos)




-- HC20T12 - File Reading with IO Monad

module Main where

-- Reads a file and prints its contents line by line
main :: IO ()
main = do
    putStrLn "Enter file name:"
    fileName <- getLine
    contents <- readFile fileName
    mapM_ putStrLn (lines contents)




-- HC20T13 - fibonacciMemo with State Monad

module Main where

import Control.Monad.State
import qualified Data.Map as M

type Memo = M.Map Int Int

-- Memoized Fibonacci
fibonacciMemo :: Int -> State Memo Int
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
    memo <- get
    case M.lookup n memo of
        Just v  -> return v
        Nothing -> do
            a <- fibonacciMemo (n - 1)
            b <- fibonacciMemo (n - 2)
            let result = a + b
            modify (M.insert n result)
            return result

-- Main function
main :: IO ()
main = do
    let (result, _) = runState (fibonacciMemo 20) M.empty
    print result



-- HC20T14 - mapMFilter

module Main where

-- Maps and filters using a monadic function
mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter _ [] = return []
mapMFilter f (x:xs) = do
    result <- f x
    rest <- mapMFilter f xs
    case result of
        Just v  -> return (v : rest)
        Nothing -> return rest

-- Example function
example :: Int -> IO (Maybe Int)
example x = do
    print x
    return (if even x then Just (x * 2) else Nothing)

-- Main function
main :: IO ()
main = do
    result <- mapMFilter example [1..6]
    print result




-- HC20T15 - treeSum with Custom Monad

module Main where

-- Binary Tree
data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)

-- Custom Sum Monad
newtype SumM a = SumM { runSumM :: a }
    deriving (Show)

instance Functor SumM where
    fmap f (SumM x) = SumM (f x)

instance Applicative SumM where
    pure = SumM
    SumM f <*> SumM x = SumM (f x)

instance Monad SumM where
    SumM x >>= f = f x

-- Sum elements in tree using custom monad
treeSum :: Tree Int -> SumM Int
treeSum Empty = return 0
treeSum (Node x left right) = do
    l <- treeSum left
    r <- treeSum right
    return (x + l + r)

-- Example tree
exampleTree :: Tree Int
exampleTree =
    Node 5
        (Node 3 Empty Empty)
        (Node 7 Empty Empty)

-- Main function
main :: IO ()
main = do
    let SumM result = treeSum exampleTree
    print result




-- HC20T16 - retryIO with IO Monad

module Main where

import Control.Exception (try, SomeException)

-- Retries an IO action up to n times
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
    result <- try action
    case result of
        Right value -> return (Just value)
        Left (_ :: SomeException) -> retryIO (n - 1) action

-- Example action
failingAction :: IO Int
failingAction = do
    putStrLn "Trying..."
    error "Failure"

-- Main function
main :: IO ()
main = do
    result <- retryIO 3 failingAction
    print result



-- HC20T17 - validatePassword with Either Monad

module Main where

-- Password validation
validatePassword :: String -> Either String String
validatePassword pwd
    | length pwd < 8 = Left "Password too short"
    | not (any (`elem` ['A'..'Z']) pwd) = Left "Must contain uppercase letter"
    | not (any (`elem` ['0'..'9']) pwd) = Left "Must contain a digit"
    | otherwise = Right pwd

-- Main function
main :: IO ()
main = do
    print (validatePassword "password")
    print (validatePassword "Password")
    print (validatePassword "Password1")



-- HC20T18 - MaybeT for User Input Validation

module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

-- Reads a number safely
readNumber :: MaybeT IO Int
readNumber = do
    liftIO $ putStrLn "Enter a number:"
    input <- liftIO getLine
    MaybeT (return (readMaybe input))

import Text.Read (readMaybe)

-- Main function
main :: IO ()
main = do
    result <- runMaybeT readNumber
    case result of
        Just n  -> putStrLn ("You entered: " ++ show n)
        Nothing -> putStrLn "Invalid input"




-- HC20T19 - Writer Monad Logging System

module Main where

import Control.Monad.Writer

-- Logs function calls
loggedAdd :: Int -> Int -> Writer [String] Int
loggedAdd x y = writer (x + y, ["add " ++ show x ++ " " ++ show y])

loggedMul :: Int -> Int -> Writer [String] Int
loggedMul x y = writer (x * y, ["mul " ++ show x ++ " " ++ show y])

-- Main function
main :: IO ()
main = do
    let (result, logs) = runWriter $ do
            a <- loggedAdd 2 3
            b <- loggedMul a 4
            return b

    putStrLn ("Result: " ++ show result)
    putStrLn "Logs:"
    mapM_ putStrLn logs




-- HC20T20 - batchProcessing with >>=

module Main where

-- Chains monadic actions using bind
batchProcessing :: Maybe Int -> Maybe Int
batchProcessing x =
    x >>= (\a ->
    Just (a * 2) >>= (\b ->
    Just (b + 10)))

-- Main function
main :: IO ()
main = do
    print (batchProcessing (Just 5))   -- Just 20
    print (batchProcessing Nothing)    -- Nothing
