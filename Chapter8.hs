-- HC8T1 - Type Synonyms and Basic Function

type Address = String
type Value   = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr amount =
    "From: " ++ fromAddr ++
    " | To: " ++ toAddr ++
    " | Value: " ++ show amount


main :: IO ()
main = do
    putStrLn "Testing generateTx:"
    print (generateTx "Alice123" "Bob456" 50)
    print (generateTx "Addr1XYZ" "Addr9LMN" 1000)
	


-- HC8T2 - New Types and Data Constructors

data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show, Eq)

data Person = Person
    { name        :: String
    , address     :: (String, Int)      -- (Street, House Number)
    , payMethod   :: PaymentMethod
    } deriving (Show, Eq)

bob :: Person
bob = Person
    { name = "Bob"
    , address = ("Main Street", 101)
    , payMethod = Cash
    }

main :: IO ()
main = do
    putStrLn "Testing Person and PaymentMethod:"
    print bob


-- HC8T3 - Algebraic Data Types and Functions

data Shape
    = Circle Float
    | Rectangle Float Float
    deriving (Show, Eq)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main :: IO ()
main = do
    let circleArea = area (Circle 5)         
    let rectArea   = area (Rectangle 10 5)   

    putStrLn "Testing Shape area function:"
    putStrLn ("Area of Circle (r=5): " ++ show circleArea)
    putStrLn ("Area of Rectangle (10x5): " ++ show rectArea)
	
	
-- HC8T4 - Record Syntax for Employee

data Employee = Employee
    { name :: String
    , experienceInYears :: Float
    } deriving (Show, Eq)

richard :: Employee
richard = Employee
    { name = "Richard"
    , experienceInYears = 7.5
    }


main :: IO ()
main = do
    putStrLn "Testing Employee record:"
    print richard
	
	
	
-- HC8T5 - Record Syntax for Person

data Person = Person
    { name        :: String
    , age         :: Int
    , isEmployed  :: Bool
    } deriving (Show, Eq)

person1 :: Person
person1 = Person
    { name = "Alice"
    , age = 30
    , isEmployed = True
    }

person2 :: Person
person2 = Person
    { name = "John"
    , age = 22
    , isEmployed = False
    }

main :: IO ()
main = do
    putStrLn "Testing Person record syntax:"
    print person1
    print person2


-- HC8T6 - Record Syntax for Shape Variants

data Shape
    = Circle
        { center :: (Float, Float)
        , color  :: String
        , radius :: Float
        }
    | Rectangle
        { width  :: Float
        , height :: Float
        , color  :: String
        }
    deriving (Show, Eq)

circle1 :: Shape
circle1 = Circle
    { center = (0.0, 0.0)
    , color  = "Red"
    , radius = 5.0
    }

rectangle1 :: Shape
rectangle1 = Rectangle
    { width  = 10.0
    , height = 5.0
    , color  = "Blue"
    }


main :: IO ()
main = do
    putStrLn "Testing Shape record syntax:"
    print circle1
    print rectangle1


-- HC8T7 - Data Types and Describing Animals

data Animal
    = Dog String   -- dog's name
    | Cat String   -- cat's name
    deriving (Show, Eq)

describeAnimal :: Animal -> String
describeAnimal (Dog name) =
    "This is a dog named " ++ name ++ "."
describeAnimal (Cat name) =
    "This is a cat named " ++ name ++ "."

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Whiskers"


main :: IO ()
main = do
    putStrLn "Testing describeAnimal:"
    putStrLn (describeAnimal dog1)
    putStrLn (describeAnimal cat1)
	
	
-- HC8T8 - Type Synonyms and Greeting Function

type Name = String
type Age  = Int

greet :: Name -> Age -> String
greet name age =
    "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."


main :: IO ()
main = do
    putStrLn "Testing greet:"
    putStrLn (greet "Alice" 25)
    putStrLn (greet "Michael" 40)
	
	
-- HC8T9 - Record Type and Transaction Function

type Address = String
type Value   = Int

data Transaction = Transaction
    { from          :: Address
    , to            :: Address
    , amount        :: Value
    , transactionId :: String
    } deriving (Show, Eq)

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
    let txId = "TX-" ++ fromAddr ++ "-" ++ toAddr ++ "-" ++ show amt
        _tx  = Transaction
                { from = fromAddr
                , to = toAddr
                , amount = amt
                , transactionId = txId
                }
    in txId


main :: IO ()
main = do
    putStrLn "Testing createTransaction:"
    print (createTransaction "Alice123" "Bob456" 50)
    print (createTransaction "AddrX" "AddrY" 999)



-- HC8T10 - Deriving Show for Book

data Book = Book
    { title  :: String
    , author :: String
    , year   :: Int
    } deriving (Show, Eq)


myBook :: Book
myBook = Book
    { title  = "Learn You a Haskell"
    , author = "Miran Lipovača"
    , year   = 2011
    }


main :: IO ()
main = do
    putStrLn "Testing Book Show instance:"
    print myBook



