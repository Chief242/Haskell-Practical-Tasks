

-- HC11T1 - Greet the User
-- Ask the user for their name, then print a greeting.

main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "! Nice to meet you.")


-- HC11T2 - Count Characters in a Line
-- Read a line from the user and print the number of characters.

main :: IO ()
main = do
    putStrLn "Enter a line of text:"
    line <- getLine
    let count = length line
    putStrLn ("Number of characters: " ++ show count)




-- HC11T3 - Double a Number
-- Ask the user for a number, read it, and print the number multiplied by 2.

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    let doubled = number * 2
    putStrLn ("The doubled number is: " ++ show doubled)



-- HC11T4 - Concatenate Two Lines
-- Read two lines from the user and print them concatenated.

main :: IO ()
main = do
    putStrLn "Enter the first line:"
    line1 <- getLine

    putStrLn "Enter the second line:"
    line2 <- getLine

    putStrLn ("Concatenated result: " ++ line1 ++ line2)



-- HC11T5 - Repeat Until "quit"
-- Repeatedly ask the user for input until they enter "quit".

loop :: IO ()
loop = do
    putStrLn "Enter something (type \"quit\" to exit):"
    input <- getLine
    if input == "quit"
        then putStrLn "Goodbye!"
        else do
            putStrLn ("You typed: " ++ input)
            loop   -- continue looping

main :: IO ()
main = loop



-- HC11T6 - Uppercase Converter
-- Read a line from the user, convert it to uppercase, and print it.

import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "Enter text to convert to uppercase:"
    input <- getLine
    let upper = map toUpper input
    putStrLn ("Uppercase: " ++ upper)



-- HC11T7 - User Options
-- Print a list of options and execute different code based on user choice.

main :: IO ()
main = do
    putStrLn "=== Menu Options ==="
    putStrLn "1. Say Hello"
    putStrLn "2. Add two numbers"
    putStrLn "3. Show current Haskell version"
    putStrLn "4. Exit"
    putStrLn "Choose an option (1–4):"

    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Hello, user!"
            main   -- show menu again

        "2" -> do
            putStrLn "Enter first number:"
            n1 <- getLine
            putStrLn "Enter second number:"
            n2 <- getLine
            let result = read n1 + read n2 :: Int
            putStrLn ("Sum: " ++ show result)
            main   -- repeat menu

        "3" -> do
            putStrLn "You are using Haskell (GHC)."
            putStrLn "Version check example: 9.x (not actual runtime value)"
            main

        "4" -> putStrLn "Goodbye!"

        _ -> do
            putStrLn "Invalid option. Try again."
            main



-- HC11T8 - Even or Odd Checker
-- Read a number from the user and print whether it is even or odd.

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let n = read input :: Int
    if even n
        then putStrLn "The number is even."
        else putStrLn "The number is odd."



-- HC11T9 - Sum Two Numbers
-- Read two numbers from the user and print their sum.

main :: IO ()
main = do
    putStrLn "Enter the first number:"
    input1 <- getLine

    putStrLn "Enter the second number:"
    input2 <- getLine

    let n1 = read input1 :: Int
    let n2 = read input2 :: Int
    let result = n1 + n2

    putStrLn ("The sum is: " ++ show result)




-- HC11T10 - Reverse User Input
-- Read a line from the user and print it reversed.

main :: IO ()
main = do
    putStrLn "Enter text to reverse:"
    input <- getLine
    let reversed = reverse input
    putStrLn ("Reversed: " ++ reversed)

