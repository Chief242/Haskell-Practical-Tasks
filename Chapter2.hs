HC2T1 - Task 1: Checking Types in GHCi
:t 42
:t 3.14
:t "Haskell"
:t 'Z'
:t True && False


42           :: Num a => a
3.14         :: Fractional a => a
"Haskell"    :: [Char]
'Z'          :: Char
True && False :: Bool



HC2T2 - Task 2: Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2
main :: IO ()
main = do
    print (add 5 7)               
    print (isEven 10)              
    print (isEven 7)               
    print (concatStrings "Hi, " "Sam!")

	
HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True


main :: IO ()
main = do
    print myAge
    print piValue
    putStrLn greeting
    print isHaskellFun
	

HC2T4 - Task 4: Converting Between Infix and Prefix Notations
main :: IO ()
main = do
    print ((+) 8 9)      
    print ((*) 20 7)     
    print ((&&) True False) 

    print (23 + 45)        
    print (9 * 6)        
    print (True && False) 
	

HC2T5 - Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)


main :: IO ()
main = do
    
    print (circleArea 12)     
    print (circleArea 20)   

    
    print (maxOfThree 10 22 6) 
    print (maxOfThree 20 45 3) 
	

HC2T6 - Task 6: Understanding Int vs Integer
smallNumber :: Int
smallNumber = 2 ^ 62  

bigNumber :: Integer
bigNumber = 2 ^ 127    

2^64 :: Int
2^64 :: Integer


HC2T7 - Task 7: Boolean Expressions
expr1 :: Bool
expr1 = True && True

expr2 :: Bool
expr2 = False || False

expr3 :: Bool
expr3 = not False

expr4 :: Bool
expr4 = 5 > 10

main :: IO ()
main = do
    print expr1  
    print expr2  
    print expr3  
    print expr4 