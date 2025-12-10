HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"


main :: IO ()
main = do
    print (weatherReport "sunny")   
    print (weatherReport "rainy")   
    print (weatherReport "cloudy")  
    print (weatherReport "stormy")  
	
	
HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

main :: IO ()
main = do
    print (dayType "Saturday")  
    print (dayType "Monday")    
    print (dayType "Funday")    
	
	
HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0  && grade <= 49  = "Better luck next time."
    | otherwise                   = "Invalid grade"

main :: IO ()
main = do
    print (gradeComment 95) 
    print (gradeComment 72)  
    print (gradeComment 60)  
    print (gradeComment 30)  
    print (gradeComment 110) 
	
	
HC4T4 - Task 4: Rewrite specialBirthday using Pattern Matching
specialBirthday :: Int -> String
specialBirthday 0  = "Welcome to the world!"
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congratulations on becoming an adult!"
specialBirthday 21 = "Cheers to 21!"
specialBirthday _  = "Another year older!"

main :: IO ()
main = do
    print (specialBirthday 0)   
    print (specialBirthday 1)   
    print (specialBirthday 18)  
    print (specialBirthday 21)  
    print (specialBirthday 30)  
	
	
HC4T5 - Task 5: Add a Catch-All Pattern with a Custom Message
specialBirthday :: Int -> String
specialBirthday 0  = "Welcome to the world!"
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congratulations on becoming an adult!"
specialBirthday 21 = "Cheers to 21!"
specialBirthday age = "Another year older: " ++ show age


main :: IO ()
main = do
    print (specialBirthday 0)   
    print (specialBirthday 1)   
    print (specialBirthday 18)  
    print (specialBirthday 21)  
    print (specialBirthday 30) 
	
	
HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [x]      = "The list has one element."
whatsInsideThisList [x, y]   = "The list has two elements."
whatsInsideThisList (x:y:_)  = "The list has more than two elements."

main :: IO ()
main = do
    print (whatsInsideThisList [])          
    print (whatsInsideThisList [5])         
    print (whatsInsideThisList [1,2])       
    print (whatsInsideThisList [1,2,3,4])   
	
	

HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> (Maybe a, Maybe a)
firstAndThird []           = (Nothing, Nothing)
firstAndThird [x]          = (Just x, Nothing)
firstAndThird [x, _]       = (Just x, Nothing)
firstAndThird (x:_:z:_)    = (Just x, Just z)

main :: IO ()
main = do
    print (firstAndThird [])           
    print (firstAndThird [1])          
    print (firstAndThird [1,2])        
    print (firstAndThird [1,2,3])      
    print (firstAndThird [5,6,7,8,9])  
	
	
	
HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (String, Int, String) -> String
describeTuple (name, age, city) =
    name ++ " is " ++ show age ++ " years old and lives in " ++ city ++ "."

main :: IO ()
main = do
    print (describeTuple ("Alice", 30, "London"))  
    print (describeTuple ("Bob", 25, "New York"))  