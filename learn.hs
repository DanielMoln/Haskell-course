-- Comment
{-
Multiline comment
-}
import Data.List
import System.IO

maxInt = maxBound :: Int
minInt = minBound :: Int

--Bool True False
--Char 'a'
always5 :: Int
always5 = 5

sumOfNums = sum [1..1000]

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

-- prefix operators
modEx = mod 5 4
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.990
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not (True)

primeNumbers = [3, 5, 7, 11]
morePrime = primeNumbers ++ [13, 17, 19]

favNums = 2 : 7 : 21 : 66 : []

multList = [[3, 5, 7], [11,13,17]]

morePrimes2 = 2 : morePrime
lenPrimes = length morePrimes2

revPrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 2
firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeInit = init morePrimes2

first3Primes = take 3 morePrimes2
first6Primes = take 6 morePrimes2
removedPrimes = drop 3 morePrimes2

is7InList = 7 `elem` morePrimes2

maxPrime = max morePrimes2
minPrime = min morePrimes2