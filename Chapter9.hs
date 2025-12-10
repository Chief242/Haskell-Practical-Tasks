-- HC9T1 - Define a Parametric Type Synonym

type Entity a = (String, a)

userEntity :: Entity String
userEntity = ("Alice", "123 Main Road")

numericEntity :: Entity Int
numericEntity = ("Sensor-01", 4421)


main :: IO ()
main = do
    putStrLn "Testing Entity type synonym:"
    print userEntity
    print numericEntity



-- HC9T2 - Implement a Parametric Data Type

data Box a
    = Empty
    | Has a
    deriving (Show, Eq)

box1 :: Box Int
box1 = Has 42

box2 :: Box String
box2 = Has "Hello"

box3 :: Box a
box3 = Empty


main :: IO ()
main = do
    putStrLn "Testing Box parametric type:"
    print box1       -- Has 42
    print box2       -- Has "Hello"
    print (box3 :: Box Int)  -- Empty
	
	
-- HC9T3 - Function to Add Values in a Box

data Box a
    = Empty
    | Has a
    deriving (Show, Eq)

addN :: Num a => a -> Box a -> Box a
addN n Empty     = Empty        
addN n (Has x)   = Has (x + n)  


box1 :: Box Int
box1 = Has 10

box2 :: Box Int
box2 = Empty


main :: IO ()
main = do
    putStrLn "Testing addN:"
    print (addN 5 box1)    -- Has 15
    print (addN 3 box2)    -- Empty
    print (addN 7 (Has 100)) -- Has 107
	
	
-- HC9T4 - Extract a Value from a Box

data Box a
    = Empty
    | Has a
    deriving (Show, Eq)


extract :: a -> Box a -> a
extract defaultValue Empty    = defaultValue
extract _           (Has x)   = x


box1 :: Box Int
box1 = Has 42

box2 :: Box Int
box2 = Empty


main :: IO ()
main = do
    putStrLn "Testing extract:"
    print (extract 0 box1)     -- 42
    print (extract 0 box2)     -- 0
    print (extract "none" (Has "hello"))  -- "hello"
    print (extract "none" (Empty :: Box String)) -- "none"
	
	
	
-- HC9T5 - Parametric Data Type with Record Syntax

data Shape a
    = Circle
        { radius :: Float
        , color  :: a
        }
    | Rectangle
        { width  :: Float
        , height :: Float
        , color  :: a
        }
    deriving (Show, Eq)

circle1 :: Shape String
circle1 = Circle
    { radius = 5.0
    , color  = "Red"
    }

rect1 :: Shape String
rect1 = Rectangle
    { width  = 10.0
    , height = 4.0
    , color  = "Blue"
    }


main :: IO ()
main = do
    putStrLn "Testing parametric Shape:"
    print circle1
    print rect1
	
	
	
-- HC9T6 - Recursive Data Type for Tweets

data Tweet = Tweet
    { content  :: String
    , likes    :: Int
    , comments :: [Tweet]
    } deriving (Show, Eq)

reply1 :: Tweet
reply1 = Tweet
    { content = "Nice post!"
    , likes = 4
    , comments = []
    }

reply2 :: Tweet
reply2 = Tweet
    { content = "I agree!"
    , likes = 2
    , comments = []
    }

mainTweet :: Tweet
mainTweet = Tweet
    { content = "Hello Haskell world!"
    , likes = 10
    , comments = [reply1, reply2]
    }


main :: IO ()
main = do
    putStrLn "Testing recursive Tweet data type:"
    print mainTweet
	
	
	
-- HC9T7 - Engagement Function for Tweets

data Tweet = Tweet
    { content  :: String
    , likes    :: Int
    , comments :: [Tweet]
    } deriving (Show, Eq)


engagement :: Tweet -> Int
engagement (Tweet _ likeCount replies) =
    likeCount + sum (map engagement replies)


reply1 :: Tweet
reply1 = Tweet
    { content = "Nice post!"
    , likes = 4
    , comments = []
    }

reply2 :: Tweet
reply2 = Tweet
    { content = "Great explanation!"
    , likes = 3
    , comments = []
    }

reply3 :: Tweet
reply3 = Tweet
    { content = "I have a question..."
    , likes = 2
    , comments = [reply1]   
    }

mainTweet :: Tweet
mainTweet = Tweet
    { content = "Learning Haskell is fun!"
    , likes = 10
    , comments = [reply2, reply3]
    }


main :: IO ()
main = do
    putStrLn "Testing engagement:"
    print (engagement reply1)       -- 4
    print (engagement reply3)       -- 2 + 4 = 6
    print (engagement mainTweet)    -- 10 + 3 + (2 + 4) = 19
	
	
	
-- HC9T8 - Recursive Sequence Data Type

data Sequence a
    = EmptySeq
    | Node a (Sequence a)
    deriving (Show, Eq)


seq1 :: Sequence Int
seq1 = Node 1 (Node 2 (Node 3 EmptySeq))

seq2 :: Sequence String
seq2 = Node "hello" (Node "world" EmptySeq)


main :: IO ()
main = do
    putStrLn "Testing Sequence recursive type:"
    print seq1
    print seq2



-- HC9T9 - Check for Element in a Sequence

data Sequence a
    = EmptySeq
    | Node a (Sequence a)
    deriving (Show, Eq)


elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ EmptySeq        = False                   
elemSeq x (Node y rest)
    | x == y              = True                    
    | otherwise           = elemSeq x rest          


seq1 :: Sequence Int
seq1 = Node 1 (Node 2 (Node 3 EmptySeq))

seq2 :: Sequence String
seq2 = Node "apple" (Node "banana" (Node "pear" EmptySeq))


main :: IO ()
main = do
    putStrLn "Testing elemSeq:"
    print (elemSeq 2 seq1)            -- True
    print (elemSeq 5 seq1)            -- False
    print (elemSeq "banana" seq2)     -- True
    print (elemSeq "orange" seq2)     -- False
	
	
	
-- HC9T10 - Binary Search Tree Data Type

data BST a
    = EmptyBST
    | Node a (BST a) (BST a)
    deriving (Show, Eq)


exampleTree :: BST Int
exampleTree =
    Node 10
        (Node 5
            (Node 2 EmptyBST EmptyBST)
            (Node 7 EmptyBST EmptyBST)
        )
        (Node 15
            (Node 12 EmptyBST EmptyBST)
            (Node 17 EmptyBST EmptyBST)
        )


main :: IO ()
main = do
    putStrLn "Testing BST data type:"
    print exampleTree







