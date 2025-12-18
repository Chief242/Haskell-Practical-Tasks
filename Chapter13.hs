-- HC13T1 - List Files in Directory
-- List all files and folders in the current directory.

import System.Directory (listDirectory)

main :: IO ()
main = do
    putStrLn "Listing all files and directories in the current folder:\n"
    files <- listDirectory "."    -- "." means current directory
    mapM_ putStrLn files



-- HC13T2 - Filter Files by Substring
-- Filter files in the current directory based on a substring.

import System.Directory (listDirectory)
import Data.List (isInfixOf)

-- Function to filter files containing a given substring
filterFiles :: String -> IO [FilePath]
filterFiles substring = do
    files <- listDirectory "."
    let filtered = filter (isInfixOf substring) files
    return filtered

main :: IO ()
main = do
    putStrLn "Enter a substring to filter files:"
    sub <- getLine

    matchedFiles <- filterFiles sub

    putStrLn "\nFiles containing the substring:"
    mapM_ putStrLn matchedFiles



-- HC13T3 - Sort and Return Filtered Files
-- Use filter and sort to return filenames that contain a substring.

import System.Directory (listDirectory)
import Data.List (filter, sort, isInfixOf)

-- Function that filters and sorts files based on a substring
sortedFilteredFiles :: String -> IO [FilePath]
sortedFilteredFiles substring = do
    files <- listDirectory "."
    let filtered = filter (isInfixOf substring) files
    let sortedList = sort filtered
    return sortedList

main :: IO ()
main = do
    putStrLn "Enter a substring to filter files:"
    sub <- getLine

    results <- sortedFilteredFiles sub

    putStrLn "\nSorted files containing the substring:"
    mapM_ putStrLn results



-- HC13T4 - SumNonEmpty Module
-- Define a function sumNonEmpty that errors on an empty list.

module SumNonEmpty
    ( sumNonEmpty
    ) where

-- sumNonEmpty: sums a list but throws an error on empty input
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = error "sumNonEmpty: cannot sum an empty list!"
sumNonEmpty xs = sum xs



-- HC13T5 - Restrict Module Export List
-- Export ONLY sumNonEmpty, keep helper functions private.

module SumNonEmpty
    ( sumNonEmpty   -- Only this function is visible to users of the module
    ) where

-- Private helper function for error message (NOT exported)
emptyListError :: a
emptyListError = error "sumNonEmpty: cannot sum an empty list!"

-- Public function
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = emptyListError   -- uses helper internally
sumNonEmpty xs = sum xs



-- HC13T6 - File Names to Map
-- Convert a list of filtered file names into a key-value Map.

import qualified Data.Map as Map

-- Convert a list of file names into a Map
-- Example value: length of the filename
fileMap :: [String] -> Map.Map String Int
fileMap names =
    Map.fromList [ (name, length name) | name <- names ]

-- Test in main
main :: IO ()
main = do
    let files = ["notes.txt", "HC13T1.hs", "data.csv"]
    let resultMap = fileMap files

    putStrLn "Map of filenames to their lengths:"
    print resultMap



H13T7

-- SumNonEmpty.hs
module SumNonEmpty
    ( sumNonEmpty
    ) where

-- Private helper (not exported)
emptyListError :: a
emptyListError = error "sumNonEmpty: cannot sum an empty list!"

-- Public function
sumNonEmpty :: Num a => [a] -> a
sumNonEmpty [] = emptyListError
sumNonEmpty xs = sum xs



-- Main.hs
-- HC13T7 - Use the SumNonEmpty module in main

import SumNonEmpty  -- import your custom module

main :: IO ()
main = do
    let nums = [5, 10, 20, 30]
    let result = sumNonEmpty nums

    putStrLn "Summing the list using sumNonEmpty:"
    print result



-- HC13T8 - Qualified Imports for Name Conflicts
-- Demonstrate resolving conflicts using qualified imports.

import qualified Data.List as L
import qualified Data.Map  as M

main :: IO ()
main = do
    let nums = [5,1,4,2,3]

    -- Using Data.List.sort via qualified import
    let sortedNums = L.sort nums

    putStrLn "Sorted numbers using Data.List.sort:"
    print sortedNums

    -- Creating a Map using qualified Data.Map
    let myMap = M.fromList [("a", 1), ("b", 2), ("c", 3)]

    putStrLn "\nApplying (+1) to values using Data.Map.map:"
    print (M.map (+1) myMap)

    putStrLn "\nDemonstration complete: qualified imports avoid naming conflicts!"



-- HC13T9 - Renaming Module Namespace
-- Demonstrate renaming imported modules and using both in one function.

import qualified Data.List as DL      -- rename Data.List to DL
import qualified Data.Map  as DM      -- rename Data.Map to DM

-- Function that demonstrates using renamed modules
demoRenamedModules :: IO ()
demoRenamedModules = do
    let nums = [9,3,1,4,7]

    -- Using Data.List (renamed DL)
    let sortedNums = DL.sort nums

    putStrLn "Sorted numbers using DL.sort:"
    print sortedNums

    -- Using Data.Map (renamed DM)
    let m = DM.fromList [("x", 10), ("y", 20)]
    let incrementedMap = DM.map (+1) m

    putStrLn "\nIncremented map values using DM.map:"
    print incrementedMap

main :: IO ()
main = demoRenamedModules



-- HC13T10 - Multi-Module Main Function
-- Use System.Directory and Data.List to search and display sorted file results.

import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)

main :: IO ()
main = do
    putStrLn "Enter a substring to search for in filenames:"
    pattern <- getLine

    -- Step 1: List files in current directory
    files <- listDirectory "."

    -- Step 2: Filter files that contain the substring
    let matchedFiles = filter (isInfixOf pattern) files

    -- Step 3: Sort the filtered results
    let sortedFiles = sort matchedFiles

    -- Step 4: Display results
    putStrLn "\nMatching files (sorted):"
    mapM_ putStrLn sortedFiles
