-- CISC 360 a2, Fall 2020

-- SEE THE FILE a2.pdf
-- for instructions

module A2 where
import Data.Char

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 20116078

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.


{-
   Q2: justify
-}
{- justify_aux column width s:
     Format s with `width' characters per line,
     ending in a newline.
     Maintain current position in `column'.

   Assume column >= 0, width >= 1.
-}
justify_aux :: Int -> Int -> [Char] -> [Char]
justify_aux column w [] = "\n"
justify_aux column w (y:ys) = if column == w then '\n' : justify_aux 0 w (y:ys)
else y : justify_aux(column+1) w ys 


{-
  justify width s: 
    Format s with `width' characters per line,
    ending in a newline.

  Assume width >= 1.
-}
justify :: Int -> [Char] -> [Char]
justify width s = justify_aux 0 width s


test_justify1 = putStr (justify 4 "123456789ABCDEF")
{- putStr prints a string in a more readable form:
   newline characters \n actually start a new line.

Expected output:

*A2> test_justify1
1234
5678
9ABC
DEF
*A2> 
-}


{-
   Q3: rewrite
-}
divisible_by_5 :: Char -> Bool
divisible_by_5 ch = (mod (ord ch) 5 == 0)

rewrite :: (Char -> Bool) -> String -> String
rewrite important [] = []
rewrite important (c : cs) = if (important c == True) then c:c:(rewrite important cs)
else (c:rewrite important cs)

test_rewrite1 = (rewrite divisible_by_5 "Queen's") == "Queenn'ss"
test_rewrite2 = (rewrite (\c -> (c > 'Z')) "sIlLy CaPiTaLiZaTiOn") == "ssIllLyy CaaPiiTaaLiiZaaTiiOnn"

{-
   Q4: lists
-}

{-
  Q4a. Fill in the definition of listCompare.
  See a2.pdf for instructions.
-}
listCompare :: [Integer] -> [Integer] -> [Bool]
listCompare []     []     = []
listCompare (x:xs) (y:ys) = if x == y then True : listCompare xs ys 
else False : listCompare xs ys
listCompare (x:xs) []     = False : listCompare xs []
listCompare []     (y:ys) = False : listCompare [] ys

test_listCompare1 = listCompare [1, 2, 4] [3, 2, 0] == [False, True, False]
test_listCompare2 = listCompare [1, 2, 1, 1] [1, 2] == [True, True, False, False]
test_listCompare3 = listCompare [1, 1] [1, 1, 1, 1] == [True, True, False, False]

{-
  Q4b.
  Briefly explain why listCompare cannot
  be implemented by

    listCompare :: [Integer] -> [Integer] -> [Bool]
    listCompare = zipWith (==)

  Write your brief explanation here:

    The code above cannot be implemented by listCompare because if the lists inputted into the
    listCompare function have different lengths the zipWith function won't be able to pad the list with falses
    and would just return a list of bools that is the length of the smaller list inputted.

  
-}

{-
  Q4c. Fill in the definition of polyCompare.
  See a2.pdf for instructions.
-}

polyCompare :: (a -> a -> Bool) -> [a] -> [a] -> [Bool]
polyCompare eq []     []     = []
polyCompare eq (x:xs) (y:ys) = if eq x y then True : polyCompare eq xs ys
else False : polyCompare eq xs ys
polyCompare eq (x:xs) []     = False : polyCompare eq xs []
polyCompare eq []     (y:ys) = False : polyCompare eq [] ys

test_polyCompare1 = polyCompare (\i -> \j -> i == j) [1, 2, 4] [3, 2, 0] == [False, True, False]

-- whoever calls polyCompare gets to define what "equal" means:
--  in test_polyCompare2, the definition of "equal" becomes whether two lists (here, strings) have the same length, regardless of the lists' contents
lengthsEqual :: [a] -> [a] -> Bool
lengthsEqual xs ys = (length xs == length ys)
test_polyCompare2 = polyCompare lengthsEqual ["a", "ab", "abcd"] ["ccc", "xy", ""] == [False, True, False]
