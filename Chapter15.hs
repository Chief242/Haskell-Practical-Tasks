HC15T1

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.IO.Error (catchIOError)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

-- ============================
-- Safe file reading with exception handling
-- ============================
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path =
    (Right <$> readFile path)
        `catchIOError` \e ->
            return (Left ("File error: " ++ show e))

-- ============================
-- Safe numeric parsing
-- ============================
readNumber :: String -> IO Double
readNumber prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input :: Maybe Double of
        Just n  -> return n
        Nothing -> do
            putStrLn "Invalid number. Try again."
            readNumber prompt

-- ============================
-- Safe velocity calculation
-- ============================
calculateVelocity :: Double -> Double -> IO Double
calculateVelocity distance time =
    if time == 0
        then ioError (userError "Time cannot be zero! Division by zero.")
        else return (distance / time)

-- ============================
-- Main program
-- ============================
main :: IO ()
main = do
    putStrLn "Enter the file path to read:"
    filePath <- getLine

    fileResult <- readFileSafe filePath
    case fileResult of
        Left err -> putStrLn err
        Right content -> do
            putStrLn "\nFile content:"
            putStrLn content

    putStrLn "\n--- Velocity Calculation ---"

    distance <- readNumber "Enter distance (meters):"
    time     <- readNumber "Enter time (seconds):"

    -- Handle velocity calculation errors
    velocityResult <- catch
        (do v <- calculateVelocity distance time
            return (Right v))
        (\(e :: SomeException) ->
            return (Left ("Velocity error: " ++ show e)))

    case velocityResult of
        Left err -> putStrLn err
        Right v  -> putStrLn ("Velocity = " ++ show v ++ " m/s")



HC15T2

-- HC15T2.hs
-- A simple self-driving AI car system that reacts to traffic light colors.

module Main where

import Data.Char (toLower)
import Text.Printf (printf)

-- Traffic light representation
data TrafficLight = Red | Yellow | Green
  deriving (Show, Eq)

-- High-level action we choose given a light
data CarAction = Stop | PrepareToStop | Go | EmergencyStop
  deriving (Show, Eq)

-- Simple car state
data CarState = CarState
  { position :: Double   -- meters from start
  , speed    :: Double   -- m/s
  , braking  :: Bool     -- whether brakes are engaged
  } deriving (Show, Eq)

-- Initial car state
initialState :: CarState
initialState = CarState { position = 0.0, speed = 15.0, braking = False }
-- Starts at 15 m/s (~54 km/h) for demonstration

-- Convert user input to TrafficLight
parseLight :: String -> Maybe TrafficLight
parseLight s =
  case map toLower (trim s) of
    "red"    -> Just Red
    "yellow" -> Just Yellow
    "amber"  -> Just Yellow
    "green"  -> Just Green
    _        -> Nothing

-- Trim whitespace helper
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Decide high-level action from light and current state
decideAction :: CarState -> TrafficLight -> CarAction
decideAction st Red
  | speed st > 0 = Stop
  | otherwise    = Stop
decideAction st Yellow
  | speed st > 8 = PrepareToStop   -- if fast, start preparing (brake)
  | otherwise    = Stop            -- slow down to stop if already slow
decideAction _ Green = Go

-- Apply action to produce a new state after dt seconds (simple physics)
-- This is intentionally simple: accel for Go, decel for Stop/PrepareToStop.
updateState :: Double -> CarState -> CarAction -> CarState
updateState dt st EmergencyStop = emergencyStop st
updateState dt st Go =
    let accel = 1.0                     -- m/s^2 comfortable acceleration
        newSpeed = clamp 0 (speed st + accel * dt) 
        newPos = position st + avgSpeed (speed st) newSpeed * dt
    in st { speed = newSpeed, position = newPos, braking = False }
updateState dt st PrepareToStop =
    let decel = -3.0                    -- m/s^2 deceleration (gentle)
        newSpeed = clamp 0 (speed st + decel * dt)
        newPos = position st + avgSpeed (speed st) newSpeed * dt
    in st { speed = newSpeed, position = newPos, braking = newSpeed < speed st }
updateState dt st Stop =
    let decel = -6.0                    -- stronger braking
        newSpeed = clamp 0 (speed st + decel * dt)
        newPos = position st + avgSpeed (speed st) newSpeed * dt
    in st { speed = newSpeed, position = newPos, braking = newSpeed < speed st }

-- Emergency stop: set speed to 0 immediately (simulated)
emergencyStop :: CarState -> CarState
emergencyStop st = st { speed = 0, braking = True }

-- Average speed helper for integrating position
avgSpeed :: Double -> Double -> Double
avgSpeed v1 v2 = (v1 + v2) / 2.0

-- Clamp helper
clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

-- Pretty print action -> message
actionMessage :: CarAction -> String
actionMessage Stop          = "Stopping now. Engage brakes."
actionMessage PrepareToStop = "Preparing to stop — easing off throttle and applying brakes gently."
actionMessage Go            = "Clear to go. Accelerating."
actionMessage EmergencyStop = "EMERGENCY STOP! Immediate braking!"

-- Run one step: decide action, update state for dt seconds, and return messages + new state
step :: Double -> CarState -> TrafficLight -> (CarAction, CarState, String)
step dt st light =
  let action = decideAction st light
      -- safety rule: if light is red but we're very near and fast, emergency stop
      safetyCheck = if light == Red && speed st > 20 && distanceToStopEstimate st < 5
                      then EmergencyStop
                      else action
      newState = updateState dt st safetyCheck
      msg = printf "Light: %s | Action: %s | Speed: %.2f m/s | Position: %.2f m"
                    (show light) (show safetyCheck) (speed newState) (position newState)
  in (safetyCheck, newState, msg)

-- Very rough stopping distance estimate: v^2 / (2 * decel)
distanceToStopEstimate :: CarState -> Double
distanceToStopEstimate st =
  let v = speed st
      decel = 6.0   -- m/s^2 assumed braking deceleration
  in (v * v) / (2 * decel)

-- Interactive loop: read traffic light colors and simulate
loop :: CarState -> IO ()
loop st = do
  putStrLn "\nEnter traffic light color (red/yellow/green) or 'quit' to exit:"
  input <- getLine
  case map toLower (trim input) of
    "quit" -> putStrLn "Simulation ended."
    txt ->
      case parseLight txt of
        Nothing -> do
          putStrLn "Invalid input. Use red, yellow, or green."
          loop st
        Just light -> do
          let dt = 1.0 -- simulate 1 second per step
              (act, st', msg) = step dt st light
          putStrLn (actionMessage act)
          putStrLn msg
          -- extra safety notification
          whenEmergency act
          loop st'

-- Helper to print emergency notice
whenEmergency :: CarAction -> IO ()
whenEmergency EmergencyStop = putStrLn "!!! EMERGENCY: immediate action taken."
whenEmergency _             = return ()

-- Entry point: demonstrate with an initial state and short automatic demo, then interactive
main :: IO ()
main = do
  putStrLn "=== Simple Self-Driving AI Car Simulator ==="
  putStrLn "Starting state:"
  print initialState

  -- Demo sequence (automated)
  let demoLights = [Green, Green, Yellow, Red, Red, Green]
  putStrLn "\nRunning demo sequence (1 second per step):"
  _ <- runSequence 1.0 initialState demoLights

  -- Interactive loop
  putStrLn "\nSwitching to interactive mode."
  loop initialState

-- Run a sequence of lights and print states
runSequence :: Double -> CarState -> [TrafficLight] -> IO CarState
runSequence _ st [] = return st
runSequence dt st (l:ls) = do
  let (act, st', msg) = step dt st l
  putStrLn (actionMessage act)
  putStrLn msg
  whenEmergency act
  runSequence dt st' ls




HC15T3

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Data.Typeable
import Data.Char (toLower)
import System.IO

-- =====================================================
-- 1. Custom exception for traffic light errors
-- =====================================================

data TrafficLightError = InvalidLight String
    deriving (Show, Typeable)

instance Exception TrafficLightError


-- =====================================================
-- 2. Traffic light type
-- =====================================================

data TrafficLight = Red | Yellow | Green
    deriving (Show, Eq)


-- =====================================================
-- 3. Parser that throws the custom exception
-- =====================================================

parseLight :: String -> IO TrafficLight
parseLight s =
    case map toLower s of
        "red"    -> return Red
        "yellow" -> return Yellow
        "green"  -> return Green
        _        -> throwIO (InvalidLight s)   -- THROW CUSTOM EXCEPTION


-- =====================================================
-- 4. Main program: ask user for a light and handle errors
-- =====================================================

main :: IO ()
main = do
    putStrLn "Enter a traffic light color (red, yellow, green):"
    input <- getLine

    -- Catch the custom exception
    result <- try (parseLight input) :: IO (Either TrafficLightError TrafficLight)

    case result of
        Left (InvalidLight bad) ->
            putStrLn ("ERROR: Invalid traffic light value: \"" ++ bad ++ "\"")

        Right light ->
            putStrLn ("Successfully parsed traffic light: " ++ show light)




HC15T4

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import Data.Typeable
import Data.Char (toLower)

-- ============================================
-- 1. Custom Traffic Light Exception
-- ============================================

data TrafficLightError = InvalidLight String
    deriving (Show, Typeable)

instance Exception TrafficLightError


-- ============================================
-- 2. Traffic Light Type
-- ============================================

data TrafficLight = Red | Yellow | Green
    deriving (Show, Eq)


-- ============================================
-- 3. Parser That Throws a Custom Exception
-- ============================================

parseLight :: String -> IO TrafficLight
parseLight s =
    case map toLower s of
        "red"    -> return Red
        "yellow" -> return Yellow
        "green"  -> return Green
        _        -> throwIO (InvalidLight s)   -- THROW custom error


-- ============================================
-- 4. Exception Handler Function
-- ============================================

handleLightError :: TrafficLightError -> IO TrafficLight
handleLightError (InvalidLight bad) = do
    putStrLn $ "Handled Exception: \"" ++ bad ++ "\" is not a valid traffic light."
    putStrLn "Defaulting to Red for safety."
    return Red  -- Safe fallback


-- ============================================
-- 5. Main Program using catch with handler
-- ============================================

main :: IO ()
main = do
    putStrLn "Enter a traffic light (red, yellow, green):"
    input <- getLine

    light <- parseLight input `catch` handleLightError

    putStrLn ("Final light value: " ++ show light)



HC15T5 

module Main where

-- Safe division: returns Nothing on divide-by-zero
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing            -- cannot divide by zero
safeDiv x y = Just (x / y)       -- safe result

main :: IO ()
main = do
    putStrLn "Enter numerator:"
    numStr <- getLine

    putStrLn "Enter denominator:"
    denStr <- getLine

    let numerator   = read numStr :: Double
        denominator = read denStr :: Double

    case safeDiv numerator denominator of
        Nothing  -> putStrLn "Error: Cannot divide by zero!"
        Just val -> putStrLn ("Result: " ++ show val)



HC15T6

module Main where

import Text.Read (readMaybe)

-- A helper that safely parses a number and retries on failure
readNumber :: IO Double
readNumber = do
    input <- getLine
    case readMaybe input :: Maybe Double of
        Just n  -> return n
        Nothing -> do
            putStrLn "Invalid number! Please try again:"
            readNumber

main :: IO ()
main = do
    putStrLn "Enter a number:"
    number <- readNumber
    putStrLn ("You entered: " ++ show number)

    putStrLn "Enter another number:"
    number2 <- readNumber
    putStrLn ("You entered: " ++ show number2)

    putStrLn ("Sum: " ++ show (number + number2))



HC15T7


-- HC15T7.hs
-- Velocity calculation with optional parsing and explicit error handling.

module Main where

import Text.Read (readMaybe)

-- Try to parse a Double from a String, returning an Either with an error message.
parseDoubleE :: String -> String -> Either String Double
parseDoubleE fieldName s =
  case readMaybe s :: Maybe Double of
    Just d  -> Right d
    Nothing -> Left $ "Invalid " ++ fieldName ++ " value: \"" ++ s ++ "\""

-- Compute velocity from distance and time given as strings.
-- Returns an error message (Left) on parse failure or invalid time (0),
-- otherwise returns Right velocity.
velocityFromStrings :: String -> String -> Either String Double
velocityFromStrings distStr timeStr = do
  dist <- parseDoubleE "distance" distStr
  time <- parseDoubleE "time" timeStr
  if time == 0
    then Left "Time cannot be zero (would cause division by zero)."
    else Right (dist / time)

-- Interactive prompt that asks for distance and time and prints result or error.
main :: IO ()
main = do
  putStrLn "Velocity calculator (velocity = distance / time)"
  putStrLn "Enter distance (meters):"
  distInput <- getLine
  putStrLn "Enter time (seconds):"
  timeInput <- getLine

  case velocityFromStrings distInput timeInput of
    Left err -> putStrLn $ "Error: " ++ err
    Right v  -> putStrLn $ "Velocity = " ++ show v ++ " m/s"



HC15T8

module Main where

-- Safe division using Either for detailed errors
safeDivE :: Double -> Double -> Either String Double
safeDivE _ 0 = Left "Error: Division by zero is not allowed."
safeDivE x y
    | isNaN x || isNaN y = Left "Error: One or both values are not valid numbers (NaN)."
    | isInfinite x || isInfinite y = Left "Error: Cannot divide infinite values."
    | otherwise = Right (x / y)

main :: IO ()
main = do
    putStrLn "Enter numerator:"
    numStr <- getLine

    putStrLn "Enter denominator:"
    denStr <- getLine

    let parsedNum = read numStr :: Double
        parsedDen = read denStr :: Double

    case safeDivE parsedNum parsedDen of
        Left err -> putStrLn err
        Right result -> putStrLn ("Result: " ++ show result)


HC15T9


module Main where

import Control.Exception (try, IOException)
import System.IO (readFile)

-- Safe file reading using try
safeReadFile :: FilePath -> IO (Either IOException String)
safeReadFile path = try (readFile path)

main :: IO ()
main = do
    putStrLn "Enter a file path to read:"
    filePath <- getLine

    result <- safeReadFile filePath

    case result of
        Left err -> do
            putStrLn "An error occurred while reading the file:"
            putStrLn ("  " ++ show err)
            putStrLn "Please check the file path and try again."

        Right content -> do
            putStrLn "\nFile read successfully! Contents:"
            putStrLn "-----------------------------------"
            putStrLn content




HC15T10


{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (try, IOException, throwIO)
import Text.Read (readMaybe)

-- Pure parsing function returning Either for clear, testable errors
parseDouble :: String -> Either String Double
parseDouble s =
  case readMaybe s :: Maybe Double of
    Just d  -> Right d
    Nothing -> Left $ "Cannot parse number from input: \"" ++ s ++ "\""

-- Pure validator that checks for semantic errors and uses Either for reporting them
validateDistanceTime :: Double -> Double -> Either String (Double, Double)
validateDistanceTime dist time
  | dist < 0  = Left "Distance cannot be negative."
  | time < 0  = Left "Time cannot be negative."
  | otherwise = Right (dist, time)

-- Compute velocity in IO, but treat division-by-zero as an IO exception (to demonstrate try/throwIO).
-- In many designs you'd keep this pure, but this shows hybrid handling.
computeVelocityIO :: Double -> Double -> IO Double
computeVelocityIO _ 0 = throwIO (userError "Runtime error: division by zero (time == 0).")
computeVelocityIO dist time = return (dist / time)

-- A helper that composes the pure Either-based parsing+validation step and then calls the IO action.
-- It returns an Either for pure errors; if pure step is OK it runs IO which may throw exceptions.
velocityHybrid :: String -> String -> IO (Either String Double)
velocityHybrid distStr timeStr =
  case parseDouble distStr of
    Left perr -> return (Left perr)                -- pure parse error
    Right dist ->
      case parseDouble timeStr of
        Left terr -> return (Left terr)            -- pure parse error
        Right time ->
          case validateDistanceTime dist time of
            Left verr -> return (Left verr)        -- pure validation error
            Right (d, t) -> do
              -- Now call the IO computation which may throw an exception (caught by caller)
              resultOrExc <- try (computeVelocityIO d t) :: IO (Either IOException Double)
              case resultOrExc of
                Left ioErr -> return (Left ("IO exception: " ++ show ioErr))
                Right v    -> return (Right v)

-- Interactive main: demonstrate both kinds of errors and success case
main :: IO ()
main = do
  putStrLn "Hybrid velocity calculator (Either for pure errors, try/IO for runtime exceptions)."
  putStrLn "Enter distance (meters):"
  dStr <- getLine
  putStrLn "Enter time (seconds):"
  tStr <- getLine

  result <- velocityHybrid dStr tStr
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right v  -> putStrLn $ "Velocity = " ++ show v ++ " m/s"

  putStrLn "\nDemo finished. Examples you can try next:"
  putStrLn "- non-numeric input (pure parsing error): 'abc' or 'one' "
  putStrLn "- negative values (pure validation error): '-5' "
  putStrLn "- zero time (IO exception translated to error message): '0' for time"
