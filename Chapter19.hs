-- HC19T1 - Applicative Instance for Pair

module Main where

-- Custom Pair data type
data Pair a = Pair a a
    deriving (Show, Eq)

-- Functor instance
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- Applicative instance
instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

-- Main function to test Pair Applicative
main :: IO ()
main = do
    let pair1 = Pair 2 3
    let pair2 = Pair 10 20

    print (pure 5 :: Pair Int)
    print (Pair (+1) (*2) <*> pair1)
    print (Pair (+) (*) <*> pair1 <*> pair2)



-- HC19T2 - addThreeApplicative Function

module Main where

import Control.Applicative (liftA3)

-- Adds three Maybe Int values using Applicative style
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

-- Main function to test addThreeApplicative
main :: IO ()
main = do
    print (addThreeApplicative (Just 1) (Just 2) (Just 3))   -- Just 6
    print (addThreeApplicative (Just 1) Nothing (Just 3))   -- Nothing



-- HC19T3 - safeProduct for Maybe Int

module Main where

import Control.Applicative (liftA2)

-- Calculates the product of a list of Maybe Int
-- Returns Nothing if any value is Nothing
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (liftA2 (*)) (Just 1)

-- Main function to test safeProduct
main :: IO ()
main = do
    print (safeProduct [Just 2, Just 3, Just 4])   -- Just 24
    print (safeProduct [Just 2, Nothing, Just 4]) -- Nothing
    print (safeProduct [])                         -- Just 1



-- HC19T4 - liftAndMultiply with liftA2

module Main where

import Control.Applicative (liftA2)

-- Lifts multiplication into an Applicative context
liftAndMultiply :: Applicative f => f Int -> f Int -> f Int
liftAndMultiply = liftA2 (*)

-- Main function to test liftAndMultiply
main :: IO ()
main = do
    print (liftAndMultiply (Just 3) (Just 4))   -- Just 12
    print (liftAndMultiply (Just 3) Nothing)    -- Nothing



-- HC19T5 - applyEffects using <*>

module Main where

-- Takes a tuple of IO Int values,
-- prints them, and returns their sum
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (ioX, ioY) =
    (\x y -> x + y)
        <$> printAndReturn ioX
        <*> printAndReturn ioY
  where
    printAndReturn action = do
        value <- action
        print value
        return value

-- Main function to test applyEffects
main :: IO ()
main = do
    result <- applyEffects (return 5, return 10)
    putStrLn ("Sum: " ++ show result)



-- HC19T6 - repeatEffect with forever

module Main where

import Control.Monad (forever)

-- Repeats an effect forever
repeatEffect :: IO () -> IO ()
repeatEffect = forever

-- Main function to test repeatEffect
main :: IO ()
main = repeatEffect $ do
    putStrLn "Enter something (Ctrl+C to stop):"
    input <- getLine
    putStrLn ("You entered: " ++ input)



-- HC19T7 - conditionalPrint using when

module Main where

import Control.Monad (when)

-- Prints a message only when the condition is True
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint condition message =
    when condition (putStrLn message)

-- Main function to test conditionalPrint
main :: IO ()
main = do
    conditionalPrint True "This will be printed"
    conditionalPrint False "This will NOT be printed"



-- HC19T8 - discardSecond using <*

module Main where

-- Sequences two effects but keeps the result of the first
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

-- Main function to test discardSecond
main :: IO ()
main = do
    result <- discardSecond
                (putStrLn "First effect" >> return 10)
                (putStrLn "Second effect" >> return 20)

    putStrLn ("Result: " ++ show result)



-- HC19T9 - pureAndApply Demonstration

module Main where

-- Demonstrates how pure works with Applicative
pureAndApply :: Maybe Int
pureAndApply = pure (+5) <*> Just 10

-- Main function to test pureAndApply
main :: IO ()
main = do
    print pureAndApply        -- Just 15
    print (pure (*2) <*> Just 7)
    print (pure (+3) <*> Nothing)



-- HC19T10 - combineResults for Either using Applicative

module Main where

-- Combines two Either values using applicative style
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = (+)

-- Main function to test combineResults
main :: IO ()
main = do
    print (combineResults (Right 5) (Right 10))     -- Right 15
    print (combineResults (Left "Error A") (Right 10))
    print (combineResults (Right 5) (Left "Error B"))
    print (combineResults (Left "Error A") (Left "Error B"))



-- HC19T11 - Applicative Instance for Wrapper

module Main where

-- Custom Wrapper data type
newtype Wrapper a = Wrapper a
    deriving (Show, Eq)

-- Functor instance
instance Functor Wrapper where
    fmap f (Wrapper x) = Wrapper (f x)

-- Applicative instance
instance Applicative Wrapper where
    pure = Wrapper
    Wrapper f <*> Wrapper x = Wrapper (f x)

-- Main function to test Wrapper Applicative
main :: IO ()
main = do
    print (pure 10 :: Wrapper Int)
    print (Wrapper (+1) <*> Wrapper 5)
    print (pure (*2) <*> Wrapper 7)



-- HC19T12 - sumThreeApplicative for Either String Int

module Main where

import Control.Applicative (liftA3)

-- Adds three Either values using applicative style
sumThreeApplicative
    :: Either String Int
    -> Either String Int
    -> Either String Int
    -> Either String Int
sumThreeApplicative = liftA3 (+)

-- Main function to test sumThreeApplicative
main :: IO ()
main = do
    print (sumThreeApplicative (Right 1) (Right 2) (Right 3))
    print (sumThreeApplicative (Right 1) (Left "Error") (Right 3))



-- HC19T13 - whenApplicative Function

module Main where

-- Executes an Applicative action only if the condition is True
whenApplicative :: Applicative f => Bool -> f () -> f ()
whenApplicative True action  = action
whenApplicative False _      = pure ()

-- Main function to test whenApplicative
main :: IO ()
main = do
    whenApplicative True (putStrLn "This will print")
    whenApplicative False (putStrLn "This will NOT print")



-- HC19T14 - replicateEffect using replicateM

module Main where

import Control.Monad (replicateM)

-- Replicates an IO action n times
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

-- Main function to test replicateEffect
main :: IO ()
main = do
    results <- replicateEffect 3 $ do
        putStrLn "Enter a number:"
        readLn
    print results



-- HC19T15 - sequenceEffects for Applicative List

module Main where

-- Combines a list of applicative effects into a single effect
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

-- Main function to test sequenceEffects
main :: IO ()
main = do
    print (sequenceEffects [Just 1, Just 2, Just 3])
    print (sequenceEffects [Just 1, Nothing, Just 3])

    results <- sequenceEffects [getLine, getLine]
    print results




-- HC19T16 - applyWithEffects using <*>

module Main where

-- Demonstrates how <*> sequences effects
applyWithEffects :: IO (Int -> Int) -> IO Int -> IO Int
applyWithEffects ioFunc ioValue = ioFunc <*> ioValue

-- Main function
main :: IO ()
main = do
    result <- applyWithEffects
        (putStrLn "Producing function" >> return (+10))
        (putStrLn "Producing value" >> return 5)

    putStrLn ("Result: " ++ show result)




-- HC19T17 - simulateMaybeEffect for multiple Maybe values

module Main where

-- Applies a function to three Maybe values using applicative style
simulateMaybeEffect :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
simulateMaybeEffect f ma mb = f <$> ma <*> mb

-- Main function
main :: IO ()
main = do
    print (simulateMaybeEffect (+) (Just 3) (Just 4))   -- Just 7
    print (simulateMaybeEffect (+) Nothing (Just 4))   -- Nothing




-- HC19T18 - combineEitherResults with multiple Either values

module Main where

-- Combines three Either computations using applicative style
combineEitherResults
    :: Either String Int
    -> Either String Int
    -> Either String Int
    -> Either String Int
combineEitherResults = liftA3 (+)

import Control.Applicative (liftA3)

-- Main function
main :: IO ()
main = do
    print (combineEitherResults (Right 1) (Right 2) (Right 3))
    print (combineEitherResults (Right 1) (Left "Error B") (Right 3))




-- HC19T19 - sequenceApplicative for Maybe list

module Main where

-- Combines a list of Maybe values into Maybe [a]
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

-- Main function
main :: IO ()
main = do
    print (sequenceApplicative [Just 1, Just 2, Just 3])
    print (sequenceApplicative [Just 1, Nothing, Just 3])




-- HC19T20 - replicateForever using forever

module Main where

import Control.Monad (forever)

-- Runs an IO action infinitely
replicateForever :: IO () -> IO ()
replicateForever = forever

-- Main function
main :: IO ()
main = replicateForever $ do
    putStrLn "Type something (Ctrl+C to stop):"
    input <- getLine
    putStrLn ("You typed: " ++ input)




