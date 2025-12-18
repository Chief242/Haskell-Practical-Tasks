-- HC17T1 - Severity Data Type and Semigroup Instance
-- Higher severity overrides lower severity.

module Main where

import Data.Semigroup (Semigroup(..))

-- Severity data type
data Severity
    = Low
    | Medium
    | High
    | Critical
    deriving (Show, Eq, Ord)

-- Semigroup instance
-- The higher severity always overrides the lower one
instance Semigroup Severity where
    (<>) = max

-- Main function to demonstrate behavior
main :: IO ()
main = do
    putStrLn "Combining severities using Semigroup (<>):"

    print (Low <> Medium)       -- Medium
    print (High <> Medium)      -- High
    print (Low <> Critical)     -- Critical
    print (High <> Critical)    -- Critical
    print (Medium <> Medium)    -- Medium




-- HC17T2 - Min and Max Newtypes with Semigroup
-- Define Min and Max newtypes for any Ord type.

module Main where

import Data.Semigroup (Semigroup(..))

-- Min newtype
newtype Min a = Min { getMin :: a }
    deriving (Show, Eq)

-- Max newtype
newtype Max a = Max { getMax :: a }
    deriving (Show, Eq)

-- Semigroup instance for Min
-- Combines values using the minimum
instance Ord a => Semigroup (Min a) where
    Min x <> Min y = Min (min x y)

-- Semigroup instance for Max
-- Combines values using the maximum
instance Ord a => Semigroup (Max a) where
    Max x <> Max y = Max (max x y)

-- Main function to demonstrate usage
main :: IO ()
main = do
    putStrLn "Min Semigroup examples:"
    print (Min 10 <> Min 5)      -- Min 5
    print (Min 3 <> Min 7)       -- Min 3

    putStrLn "\nMax Semigroup examples:"
    print (Max 10 <> Max 5)      -- Max 10
    print (Max 3 <> Max 7)       -- Max 7

    putStrLn "\nExtracting values:"
    print (getMin (Min 8 <> Min 2))
    print (getMax (Max 4 <> Max 9))



-- HC17T3 - Monoid Instance for Severity
-- Identity value (mempty) is Low.

module Main where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

-- Severity data type
data Severity
    = Low
    | Medium
    | High
    | Critical
    deriving (Show, Eq, Ord)

-- Semigroup instance
-- Higher severity overrides lower severity
instance Semigroup Severity where
    (<>) = max

-- Monoid instance
-- Identity value is Low
instance Monoid Severity where
    mempty = Low
    mappend = (<>)

-- Main function to demonstrate behavior
main :: IO ()
main = do
    putStrLn "Semigroup examples:"
    print (Low <> Medium)        -- Medium
    print (High <> Medium)       -- High
    print (Critical <> Low)      -- Critical

    putStrLn "\nMonoid examples:"
    print (mempty <> High)       -- High
    print (Low <> mempty)        -- Low
    print (mconcat [Low, Medium, High, Medium]) -- High


-- HC17T4 - Monoid Instance for Sum Newtype
-- Identity element is 0

module Main where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

-- Sum newtype
newtype Sum a = Sum { getSum :: a }
    deriving (Show, Eq)

-- Semigroup instance
-- Combines values using addition
instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

-- Monoid instance
-- Identity value is 0
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend = (<>)

-- Main function to demonstrate usage
main :: IO ()
main = do
    putStrLn "Semigroup examples:"
    print (Sum 10 <> Sum 5)       -- Sum 15
    print (Sum 3 <> Sum 7)        -- Sum 10

    putStrLn "\nMonoid examples:"
    print (mempty <> Sum 8)       -- Sum 8
    print (Sum 8 <> mempty)       -- Sum 8
    print (mconcat [Sum 1, Sum 2, Sum 3, Sum 4]) -- Sum 10

    putStrLn "\nExtracted values:"
    print (getSum (Sum 20 <> Sum 22)) -- 42


-- HC17T5 - combineLists Function
-- Uses the Semigroup instance to concatenate two lists of integers.

module Main where

import Data.Semigroup (Semigroup(..))

-- Function to combine two lists using Semigroup
combineLists :: [Int] -> [Int] -> [Int]
combineLists xs ys = xs <> ys

-- Main function to demonstrate usage
main :: IO ()
main = do
    let list1 = [1, 2, 3]
    let list2 = [4, 5, 6]

    putStrLn "First list:"
    print list1

    putStrLn "Second list:"
    print list2

    putStrLn "Combined list:"
    print (combineLists list1 list2)



-- HC17T6 - maxSeverity Function
-- Combines a list of Severity values using mconcat.

module Main where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

-- Severity data type
data Severity
    = Low
    | Medium
    | High
    | Critical
    deriving (Show, Eq, Ord)

-- Semigroup instance
-- Higher severity overrides lower severity
instance Semigroup Severity where
    (<>) = max

-- Monoid instance
-- Identity value is Low
instance Monoid Severity where
    mempty = Low
    mappend = (<>)

-- Function to get the maximum severity from a list
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- Main function to demonstrate usage
main :: IO ()
main = do
    let severities1 = [Low, Medium, High, Medium]
    let severities2 = [Low, Low, Medium]
    let severities3 = [Critical, High, Medium]

    putStrLn "Maximum severity in severities1:"
    print (maxSeverity severities1)   -- High

    putStrLn "Maximum severity in severities2:"
    print (maxSeverity severities2)   -- Medium

    putStrLn "Maximum severity in severities3:"
    print (maxSeverity severities3)   -- Critical

    putStrLn "Maximum severity of an empty list:"
    print (maxSeverity [])            -- Low (mempty)



-- HC17T7 - multiplyProducts Function
-- Combines a list of Product values using mconcat.

module Main where

import Data.Monoid (Product(..))

-- Function to multiply a list of Product values
multiplyProducts :: Num a => [Product a] -> Product a
multiplyProducts = mconcat

-- Main function to demonstrate usage
main :: IO ()
main = do
    let products1 = [Product 2, Product 3, Product 4]
    let products2 = [Product 5, Product 10]
    let products3 = []  -- empty list

    putStrLn "Multiplying products1:"
    print (multiplyProducts products1)      -- Product 24

    putStrLn "Multiplying products2:"
    print (multiplyProducts products2)      -- Product 50

    putStrLn "Multiplying empty list:"
    print (multiplyProducts products3)      -- Product 1

    putStrLn "Extracted numeric results:"
    print (getProduct (multiplyProducts products1)) -- 24
    print (getProduct (multiplyProducts products2)) -- 50



-- HC17T8 - foldWithSemigroup Function
-- Combines elements of a list using foldr and the Semigroup operation.

module Main where

import Data.Semigroup (Semigroup(..))

-- Function that folds a non-empty list using Semigroup
foldWithSemigroup :: Semigroup a => a -> [a] -> a
foldWithSemigroup initial xs = foldr (<>) initial xs

-- Main function to demonstrate usage
main :: IO ()
main = do
    putStrLn "Using foldWithSemigroup with lists:"
    print (foldWithSemigroup [1,2] [[3,4], [5,6]])
    -- Result: [1,2,3,4,5,6]

    putStrLn "\nUsing foldWithSemigroup with Sum:"
    let sums = [Sum 10, Sum 20, Sum 30]
    print (foldWithSemigroup (Sum 0) sums)
    -- Result: Sum 60

    putStrLn "\nUsing foldWithSemigroup with Product:"
    let products = [Product 2, Product 3, Product 4]
    print (foldWithSemigroup (Product 1) products)
    -- Result: Product 24

-- Needed imports for Sum and Product
import Data.Monoid (Sum(..), Product(..))




-- HC17T9 - Config Data Type and Semigroup Instance
-- Combines configurations using max for loggingLevel and retries,
-- and min for timeout.

module Main where

import Data.Semigroup (Semigroup(..))

-- Logging level data type
data LoggingLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord)

-- Config data type
data Config = Config
    { loggingLevel :: LoggingLevel
    , timeout      :: Int     -- in seconds
    , retries      :: Int
    }
    deriving (Show, Eq)

-- Semigroup instance for Config
instance Semigroup Config where
    Config l1 t1 r1 <> Config l2 t2 r2 =
        Config
            { loggingLevel = max l1 l2   -- higher logging level wins
            , timeout      = min t1 t2   -- lower timeout wins
            , retries      = max r1 r2   -- higher retry count wins
            }

-- Main function to demonstrate usage
main :: IO ()
main = do
    let configA = Config
            { loggingLevel = Info
            , timeout = 30
            , retries = 2
            }

    let configB = Config
            { loggingLevel = Error
            , timeout = 10
            , retries = 5
            }

    let configC = Config
            { loggingLevel = Debug
            , timeout = 60
            , retries = 1
            }

    putStrLn "Config A:"
    print configA

    putStrLn "\nConfig B:"
    print configB

    putStrLn "\nConfig C:"
    print configC

    putStrLn "\nCombined Config (A <> B <> C):"
    print (configA <> configB <> configC)



-- HC17T10 - Monoid Instance for Config
-- Identity element:
--   lowest loggingLevel (Debug)
--   highest timeout (maxBound)
--   lowest retries (0)

module Main where

import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

-- Logging level data type
data LoggingLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord, Bounded)

-- Config data type
data Config = Config
    { loggingLevel :: LoggingLevel
    , timeout      :: Int     -- in seconds
    , retries      :: Int
    }
    deriving (Show, Eq)

-- Semigroup instance
-- Combine by:
--   max loggingLevel
--   min timeout
--   max retries
instance Semigroup Config where
    Config l1 t1 r1 <> Config l2 t2 r2 =
        Config
            { loggingLevel = max l1 l2
            , timeout      = min t1 t2
            , retries      = max r1 r2
            }

-- Monoid instance
-- Identity configuration
instance Monoid Config where
    mempty =
        Config
            { loggingLevel = minBound   -- Debug
            , timeout      = maxBound   -- very large timeout
            , retries      = 0
            }

    mappend = (<>)

-- Main function to demonstrate usage
main :: IO ()
main = do
    let configA = Config Info 30 2
    let configB = Config Error 10 5

    putStrLn "Config A:"
    print configA

    putStrLn "\nConfig B:"
    print configB

    putStrLn "\nIdentity Config (mempty):"
    print (mempty :: Config)

    putStrLn "\nCombined Config (A <> B):"
    print (configA <> configB)

    putStrLn "\nCombining with identity:"
    print (configA <> mempty)
    print (mempty <> configA)

    putStrLn "\nCombining multiple configs using mconcat:"
    print (mconcat [configA, configB, mempty])
