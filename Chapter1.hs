HC1T1 - Task 1: Function Composition
double:: Int -> Int
double x = x * 2

increment:: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main :: IO ()
main = do
    print(doubleThenIncrement 3)
    print(doubleThenIncrement 10)


HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a ->a
circleArea r = pi * r * r

main :: IO ()
main = do 
  print (circleArea 5)
  print (circleArea 20)
  
HC1T3 - Task 3: Checking if a Number is Greater than 18
  
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main :: IO ()
main = do
    print(greaterThan18 10)
    print(greaterThan18 18)
    print(greaterThan18 19)
  
HC1T4 - Task 4: Composing a Function to Process Player Data
import Data.List (sortBy)
import Data.Ord (comparing)

extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main :: IO ()
main = do
    let players = [("Alice", 50), ("Bob", 75), ("Charlie", 60), ("David", 80), ("Eve", 70)]
    print (getTopThreePlayers players)
    -- Output: ["David","Bob","Eve"]


  HC1T5 - Task 5: Laziness in Haskell
  
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers

main :: IO ()
main = do
  print  (firstN 2)
  print  (firstN 16)
  
  
HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main :: IO ()
main = do
    print(addNumbers 8 9)
    print(addNumbers 20 21)
  
  
HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * (5 / 9)

main :: IO ()
main = do
    print(fToC 32)
    print(fToC 212)
    print(fToC 98.6)
  
HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

double :: Int -> Int
double x = x * 2

main :: IO ()
main = do
    print (applyTwice double 3)    
    print (applyTwice (+1) 5)      
    print (applyTwice reverse "abc")
