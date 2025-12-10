-- HC10T1 - ShowSimple Type Class

data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show, Eq)

class ShowSimple a where
    showSimple :: a -> String

instance ShowSimple PaymentMethod where
    showSimple Cash           = "Paid with cash"
    showSimple Card           = "Paid with card"
    showSimple Cryptocurrency = "Paid with cryptocurrency"


main :: IO ()
main = do
    putStrLn "Testing ShowSimple instance for PaymentMethod:"
    putStrLn (showSimple Cash)
    putStrLn (showSimple Card)
    putStrLn (showSimple Cryptocurrency)
	
	
	
-- HC10T2 - Summable Type Class

class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = foldr (+) 0


main :: IO ()
main = do
    putStrLn "Testing Summable for Int:"
    print (sumUp [1,2,3,4,5])   -- 15
    print (sumUp [10,20,30])    -- 60
    print (sumUp [])            -- 0
	
	

-- HC10T3 - Comparable Type Class

data Blockchain = Blockchain
    { chainName   :: String
    , blockHeight :: Int
    } deriving (Show, Eq)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith bc1 bc2 =
        compare (blockHeight bc1) (blockHeight bc2)

btc :: Blockchain
btc = Blockchain "Bitcoin" 840000

eth :: Blockchain
eth = Blockchain "Ethereum" 21000000


main :: IO ()
main = do
    putStrLn "Testing Comparable for Blockchain:\n"
    print (compareWith btc eth)   -- LT, GT, or EQ depending on heights
    print (compareWith eth btc)
    print (compareWith btc btc)
	
	
	
-- HC10T4 - Eq Instance for Box

data Box a
    = Empty
    | Has a
    deriving (Show)


instance Eq a => Eq (Box a) where
    Empty   == Empty   = True
    Has x   == Has y   = x == y
    _       == _       = False


box1 :: Box Int
box1 = Has 10

box2 :: Box Int
box2 = Has 10

box3 :: Box Int
box3 = Has 5

box4 :: Box Int
box4 = Empty


main :: IO ()
main = do
    putStrLn "Testing Eq instance for Box:\n"
    print (box1 == box2)   -- True
    print (box1 == box3)   -- False
    print (box1 == box4)   -- False
    print (box4 == Empty)  -- True
	
	
	
-- HC10T5 - ShowDetailed Type Class

class ShowDetailed a where
    showDetailed :: a -> String


data User = User
    { username :: String
    , age      :: Int
    , isAdmin  :: Bool
    } deriving (Show, Eq)


instance ShowDetailed User where
    showDetailed (User name age adminStatus) =
        "User Details:\n"
        ++ "- Username: " ++ name ++ "\n"
        ++ "- Age: " ++ show age ++ "\n"
        ++ "- Admin: " ++ (if adminStatus then "Yes" else "No")


user1 :: User
user1 = User "Alice" 25 False

user2 :: User
user2 = User "Bob" 40 True


main :: IO ()
main = do
    putStrLn "Testing ShowDetailed instance for User:\n"
    putStrLn (showDetailed user1)
    putStrLn ""
    putStrLn (showDetailed user2)
	
	
	
-- HC10T6 - Mutual-recursive style Eq instance for Blockchain

module Main where

data Blockchain = Blockchain
    { chainName   :: String
    , blockHeight :: Int
    } deriving (Show)


instance Eq Blockchain where
    x == y = not (x /= y)


    x /= y = chainName x /= chainName y || blockHeight x /= blockHeight y


btc :: Blockchain
btc = Blockchain "Bitcoin" 840000

btcSame :: Blockchain
btcSame = Blockchain "Bitcoin" 840000

eth :: Blockchain
eth = Blockchain "Ethereum" 21000000

main :: IO ()
main = do
    putStrLn "Testing Eq instance for Blockchain (mutual-style):"
    putStrLn $ "btc == btcSame    : " ++ show (btc == btcSame)   -- True
    putStrLn $ "btc /= btcSame    : " ++ show (btc /= btcSame)   -- False
    putStrLn $ "btc == eth        : " ++ show (btc == eth)       -- False
    putStrLn $ "btc /= eth        : " ++ show (btc /= eth)       -- True




-- HC10T7 - Convertible Type Class

data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show, Eq)


class Convertible a b where
    convert :: a -> b


instance Convertible PaymentMethod String where
    convert Cash           = "Cash payment"
    convert Card           = "Card payment"
    convert Cryptocurrency = "Cryptocurrency payment"


main :: IO ()
main = do
    putStrLn "Testing Convertible for PaymentMethod -> String:"
    putStrLn (convert Cash)
    putStrLn (convert Card)
    putStrLn (convert Cryptocurrency)



-- HC10T8 - AdvancedEq Subclass of Eq

class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool


data PaymentMethod = Cash | Card | Cryptocurrency
    deriving (Show, Eq)


instance AdvancedEq PaymentMethod where
    compareEquality x y = x == y


main :: IO ()
main = do
    putStrLn "Testing AdvancedEq:"
    print (compareEquality Cash Cash)               -- True
    print (compareEquality Cash Card)               -- False
    print (compareEquality Cryptocurrency Cash)     -- False
	
	
	
-- HC10T9 - MinMax Type Class

class MinMax a where
    minValue :: a
    maxValue :: a


instance MinMax Int where
    minValue = minBound   -- minimum Int value
    maxValue = maxBound   -- maximum Int value


main :: IO ()
main = do
    putStrLn "Testing MinMax for Int:"
    putStrLn $ "Min Int value: " ++ show (minValue :: Int)
    putStrLn $ "Max Int value: " ++ show (maxValue :: Int)
	
	
	
-- HC10T10 - Concatenatable Type Class

class Concatenatable a where
    concatWith :: a -> a -> a


instance Concatenatable String where
    concatWith s1 s2 = s1 ++ s2


main :: IO ()
main = do
    putStrLn "Testing Concatenatable for String:"
    putStrLn (concatWith "Hello, " "World!")
    putStrLn (concatWith "Haskell " "Rocks!")







