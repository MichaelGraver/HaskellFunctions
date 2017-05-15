--Simple Haskell functions for a University Assignment



{-Question 1 

Write a function inlist which, given a list of Integers and an Integer n, 
returns a Boolean indicating whether n occurs in the list.

Main>  inlist [2,3,2,4,7,9] 7
True
Main> inlist [2..100] 101
False
Main> inlist [2..] 101
True

-}
inlist :: [Int] -> Int -> Bool

inlist [] x = False
{-Uses recursion to go through the list
if the head of the list = the input return true-}
inlist (y : ys) x
    | y == x = True
    | otherwise = inlist ys x


{-Question 2

Write a function exactlyonce which takes a list of Integers and an Integer n,
then returns a Boolean indicating whether n occurs exactly once in the list. 
Can you give a function that produces a result which includes the last example?
If not, why not? Explain in a couple of sentences.

If the list is infinte it would be unable to ever know if the number would appear
again as it would never reach the end.

Main> exactlyonce [2,3,2,4,3] 3
False
Main> 
exactlyonce [3..30] 15 
True
Main> 
exactlyonce [1..] 15 
It does not return anything as it is still searching

-}
exactlyonce :: [Int] -> Int -> Bool

exactlyonce [] x = False
{-Filters out everything except input if length of 
remaining list = the input then returns true-}
exactlyonce ys x = length (filter (== x) ys) == 1


{-Question 3

Define a function equalones which takes two lists of integers and decides whether
it's true that each of the lists contain equal numbers of ones.

Main equalones [1,2,0] [3,5,1,1]
False
Main equalones [1,0] [0]
False
Main equalones [1,0,0,1] [0,1,1,0]
True

-}
equalones :: [Int] -> [Int] -> Bool

{-Two empty list should probably return true as the amount of 1s
is technically equal but I have made it equal false
to check for empty lists instead-}
equalones [] [] = False
{-Takes 2 lists and filers out everything but the 1s
then it counts the lenght and compares the two to see if they are equal-}
equalones list1 list2 = (filter (==1) list1) == (filter (==1) list2)



{-Question 4

Write a function replace new which takes an integer x and a list of integers.
It returns a list where each element y of the list equals (x-y)*(x-y).

Main> replacenew 2 [3,6,9]
[1,16,49]

-}
replacenew :: Int -> [Int] -> [Int]

replacenew x [] = []
{-Uses recursion to move through the list applying the sum
to each element -}
replacenew x (y : ys) = ((x-y)*(x-y)) : replacenew x ys


{-Question 5

Define a function addthemup that takes a list of lists of integers, 
sums the numbers in each of the lists within the list, then multiplies the
resulting sums with each other.

Main addthemup [[1,3],[3,7]]
40
Main addthemup [[1,2,3],[9]]
54
Main addthemup [[1,2],[]]
0
Main> addthemup [[1,2],[1,3],[4,5,7],[2]]
384

-}
addthemup :: [[Int]] -> Int

addthemup [] = 0
--Adds up the individual lists and then multiplies the results
addthemup (x:xs) = foldr (*) 1 (map sum (x:xs))
     where sum ys = foldr (+) 0 ys



{-Question 6

Define a function repeat new that repeats the application of 
a function to an argument a given number of times. 

Main> repeatnew square 1 2
4 
Main> repeatnew square 2 2 
16
Main> repeatnew square 4 1 
1
Main> repeatnew square 10 0.999
0.3589714781897133

-}
square :: (Num y) => y -> y

square y = y * y


--cannot be negative
repeatnew :: (a -> a) -> Int -> a -> a
--Takes in a function and applies it as meany times as specified
repeatnew _ 0 x   = x
repeatnew f 1 x   = f x
repeatnew f num x = repeatnew f (num - 1) (f x)


{-Question 7

Define a function antepenultimate1 such that, if xs is a list of integers 
then antepenultimate1 xs is True if the antepenultimate member of xs is 1.

Main> f2 []
False
Main> f2 [1,0,1,1,0]
True
Main> f2 [1,0,0,1,1]
False
Main> f2 [1,0,1,0,1,0]
False

-}
antepenultimate1 :: [Int] -> Bool

antepenultimate1 [] = False
{-Finds the end of the list and then works out what the
3rd last number is and if it equals 1-}
antepenultimate1 (x : _ : _ : []) = x == 1
antepenultimate1 (_ : xs)     = antepenultimate1 xs


{-Question 8

Define a function sequenceones such that sequenceones xs = True
if and only if xs contains the substring 11.

Main> sequenceones []
False
Main> sequenceones [1,0,1,1,0]
True
Main> sequenceones [1,0,0,1,0,1]
False
Main> sequenceones [1,0,1,0,1]
False

-}
sequenceones :: [Int] -> Bool

sequenceones [] = False
--Finds any 1 followed by a 1 and returns true if one is found
sequenceones (1 : 1 : _) = True
sequenceones (_ : xs) = sequenceones xs