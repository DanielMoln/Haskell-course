module Learn2 where
import Data.List
import Data.ByteString.Char8 (hPutStrLn)
import GHC.IO
import GHC.IO.IOMode
import System.IO (openFile, hClose, hPutStr, hGetContents)
import Distribution.Utils.LogProgress (dieProgress)

val = True :: Bool

num = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num)

primeNumbers = [3, 5, 7, 11]
morePrimes = primeNumbers ++ [13, 17, 19]
favNums = 2 : 3 : 1 : 4 : 9 : 7 : []
morePrimes2 = 2 : morePrimes
lenOfPrimes = length morePrimes
reversePrimes = reverse morePrimes
isPrimesEmpty = null morePrimes
secondPrime = morePrimes !! 2
firstPrime = head morePrimes
lastPrimes = last morePrimes
primeInit = init morePrimes

firstThree = take 3 morePrimes
removedThree = drop 3 morePrimes

isSevenInList = 7 `elem` morePrimes2

zeroToTen = [0..10]
evenList = [2,4..20]

prodList = product evenList

letterList = ['A', 'B'..'X']
--infinPow10 = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1, 2, 3, 4, 5])

listTimes2 = [ x * 2 | x <- [1, 2, 3, 4, 5], x * 3 <= 50]

divisBy9N13 = [ x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0 ]

sortedList = sort [9, 7, 3, 4, 5, 6]

sumOfLists = zipWith (+) [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]

listBiggerThen5 = filter (>5) morePrimes

evensUpTo20 = takeWhile (<=20) [2,4..]

multOfList = foldl (*) 1 [2, 3, 4, 5]

addedList = foldl (+) 0 [2,4..20]

pow3List = [ 3 ^ n | n <- [1..10]]

multTable = [[ x * y | y <- [1..10]] | x <- [1..10]]

randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52)
bobsName = fst bobSmith -- first value
bobsAge = snd bobSmith -- last value

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesNAddresses = zip names addresses

addMe :: Int -> Int -> Int
addMe x y = x + y

sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge x = "Nothing important"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | otherwise = "Go to college"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Avarage"
    | avg <= 0.250 = "Avarage Player"
    | avg <= 0.280 = "Your doing pretty good"
    | otherwise = "You're a Superstart"
    where avg = hits / atBats

calculateCanDrive :: Int -> String
calculateCanDrive age
    | able == True = "Can drive"
    | able == False = "Can't drive"
    where able = age > 17

getListItems :: [Int] -> String

getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty string"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1..5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

areStringEqual  :: [Char] -> [Char] -> Bool
areStringEqual [] [] = True
areStringEqual (x:xs) (y:ys) = x == y && areStringEqual xs ys
areStringsEqual _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3

num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3

dbl1To10 = map (\x -> x * 2) [1..10]

doubleEvenNumber y =
    if (y `mod` 2 /= 0)
        then y
        else y * 2

getClass :: Int -> String
getClass n = case n of
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away"

data BaseballPlayer = Pitcher
                      | Catcher
                      | Infielder
                      | Outfield
                      deriving Show
                    
barryBonds :: BaseballPlayer -> Bool
barryBonds Catcher = True
barryInOf = print(barryBonds Catcher)

isJohnCatcher :: BaseballPlayer -> Bool
isJohnCatcher x = case x of
    Catcher -> True
    _ -> False

data Customer = Customer String String Double
    deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats rock! Win!"
shoot Paper Paper = "draw!"
shoot Paper Scissors = "Scissors beat paper! Lost!"
shoot Rock Paper = "Paper beat rock! Lost!"
-- and so on.. I am just lazy
shoot _ _ = "Error"

                  -- x     y    radius
data Shape = Circle Float Float Float
                    --   top left corner, top right, bottom left, bottom right
             | Rectangle Float Float Float Float

area :: Shape -> Float

area (Circle _ _ r) = pi * r ^2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)

sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100

data Employee = Employee { 
    name :: String,
    position :: String,
    idNum :: Int                           
} deriving (Eq, Show)

samSmith = Employee {
    name = "Sam Smith",
    position = "Manager",
    idNum = 1000
}

pamMarx = Employee {
    name = "Pam Marx",
    position = "Sales",
    idNum = 1001
}

isSamPam = samSmith == pamMarx

samSmithData = show samSmith

data ShirtSize = S | M | L
instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where
    show S = "Small"
    show M = "Medium"
    show L = "Large"

class MyEq a where
    areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
    areEqual S S = True
    areEqual M M = True
    areEqual L L = True
    areEqual _ _ = False

sayHello = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name

writeToFile = do
    theFile <- openFile "test.txt" WriteMode
    hPutStr theFile ("Random line of text")
    hClose theFile

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

newSize = areEqual M M

main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Your name is: " ++ name)
