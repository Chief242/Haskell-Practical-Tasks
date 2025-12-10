-- HC7T1 - Implement an Eq Instance for a Custom Data Type

data Color = Red | Green | Blue

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False    


main :: IO ()
main = do
    putStrLn "Testing Color Eq instance:"
    print (Red == Red)       -- True
    print (Red == Green)     -- False
    print (Blue == Blue)     -- True
    print (Green == Blue)    -- False
	

-- HC7T2 - Implement an Ord Instance for a Custom Data Type

data Color = Red | Green | Blue
    deriving (Eq, Show)  
   
instance Ord Color where
    Red   <= _     = True
    Green <= Blue  = True
    Green <= Green = True
    Blue  <= Blue  = True
    Red   <= Red   = True
    _     <= _     = False


main :: IO ()
main = do
    putStrLn "Testing Color Ord instance:"
    print (Red < Green)     -- True
    print (Green < Blue)    -- True
    print (Red < Blue)      -- True
    print (Blue < Red)      -- False
    print (Green >= Red)    -- True



-- HC7T3 - Function Using Multiple Constraints

compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y =
    if x >= y then x else y


main :: IO ()
main = do
    putStrLn "Testing compareValues:"
    print (compareValues 10 20)         -- 20
    print (compareValues 5 5)           -- 5
    print (compareValues 'a' 'z')       -- 'z'
    print (compareValues "hi" "hello")  -- "hi"   (lexicographic)
	
	
	
-- HC7T4 - Custom Type with Show and Read

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Eq)

instance Show Shape where
    show (Circle r) =
        "Circle " ++ show r
    show (Rectangle w h) =
        "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ("Circle" : r : _) ->
                [(Circle (read r), "")]
            ("Rectangle" : w : h : _) ->
                [(Rectangle (read w) (read h), "")]
            _ -> []   -- parsing failed


main :: IO ()
main = do
    putStrLn "Testing Show instance:"
    print (Circle 5.0)
    print (Rectangle 3.0 4.5)

    putStrLn "\nTesting Read instance:"
    print (read "Circle 5.0" :: Shape)
    print (read "Rectangle 3.0 4.5" :: Shape)
	
	
	
-- HC7T5 - Function with Num Constraint

squareArea :: Num a => a -> a
squareArea side = side * side


main :: IO ()
main = do
    putStrLn "Testing squareArea:"
    print (squareArea (5 :: Int))       -- 25
    print (squareArea (3.5 :: Double))  -- 12.25
    print (squareArea (10 :: Integer))  -- 100
	
	

-- HC7T6 - Using Integral and Floating Type Classes

circleCircumference :: (Integral a, Floating b) => a -> b
circleCircumference r =
    2 * pi * fromIntegral r


main :: IO ()
main = do
    putStrLn "Testing circleCircumference:"
    print (circleCircumference (5 :: Int)    :: Double)   -- 31.4159...
    print (circleCircumference (10 :: Integer) :: Float)  -- 62.8318...
    print (circleCircumference (7 :: Int)    :: Double)   -- 43.9822...
	
	

-- HC7T7 - Bounded and Enum

data Color = Red | Green | Blue
    deriving (Show, Eq, Enum, Bounded)

nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound   
    | otherwise     = succ c     


main :: IO ()
main = do
    putStrLn "Testing nextColor:"
    print (nextColor Red)     -- Green
    print (nextColor Green)   -- Blue
    print (nextColor Blue)    -- Red (wraps around)
	
	
-- HC7T8 - Parse a Value from a String Using Read

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Show, Read)

parseShape :: String -> Maybe Shape
parseShape str =
    case reads str :: [(Shape, String)] of
        [(shape, "")] -> Just shape    
        _             -> Nothing       

main :: IO ()
main = do
    putStrLn "Testing parseShape:"
    print (parseShape "Circle 5.0")              -- Just (Circle 5.0)
    print (parseShape "Rectangle 3.0 4.5")       -- Just (Rectangle 3.0 4.5)
    print (parseShape "Triangle 3.0 4.0 5.0")    -- Nothing
    print (parseShape "Circle five")             -- Nothing
    print (parseShape "Circle 5.0 extra")        -- Nothing



-- HC7T9 - Type Class with Multiple Instances

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Show, Read)

class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "This is True."
    describe False = "This is False."

instance Describable Shape where
    describe (Circle r) =
        "A circle with radius " ++ show r
    describe (Rectangle w h) =
        "A rectangle with width " ++ show w ++
        " and height " ++ show h


main :: IO ()
main = do
    putStrLn "Testing Describable instances:"
    
    print (describe True)
    print (describe False)

    print (describe (Circle 5.0))
    print (describe (Rectangle 3.0 4.5))
	
	
	
-- HC7T10 - Function with Multiple Type Class Constraints

class Describable a where
    describe :: a -> String

data Shape
    = Circle Double
    | Rectangle Double Double
    deriving (Show, Read, Ord, Eq)

instance Describable Shape where
    describe (Circle r) =
        "A circle with radius " ++ show r
    describe (Rectangle w h) =
        "A rectangle with width " ++ show w ++
        " and height " ++ show h

instance Describable Bool where
    describe True  = "This is True."
    describe False = "This is False."

describeAndCompare :: (Ord a, Describable a) => a -> a -> String
describeAndCompare x y =
    if x >= y
        then describe x
        else describe y


main :: IO ()
main = do
    putStrLn "Testing describeAndCompare:\n"

    print (describeAndCompare True False)      -- True is larger, describes True

    -- Using Shape (compare by derived Ord)
    let s1 = Circle 5.0
    let s2 = Rectangle 3 4

    print (describeAndCompare s1 s2)






