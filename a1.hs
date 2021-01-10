-- CISC 360 a1, Fall 2020

module A1 where

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 20116078

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE


-- Q2.1: between

-- between m n p == True if and only if n is between m and p
-- (not *strictly* between:  between 1 1 4 and between 10 300 300 should return True)
--
between :: Integer -> Integer -> Integer -> Bool
between m n p =
   if (m<=n) && (n<=p) then True
      else False
   
-- Testing between:
--
-- Test cases for between
test_between1, test_between2, test_between3, test_between4, test_between5 :: Bool
test_between1 = between 1 2 3
test_between2 = not (between 1 0 5)
test_between3 = between (-2) (-2) (-1)
test_between4 = between 1 2 2
test_between5 = not (between 0 30 29)
-- Do all 5 tests together
test_between :: Bool
test_between  = test_between1 && test_between2
                              && test_between3
                              && test_between4
                              && test_between5

-- Q2.2: carry
--
-- Given two nonnegative integers m, n:
-- carry m n  returns  1  if both m and n are odd,
--                     0  otherwise
--
-- Hint: mod k 2 returns 1 if k is odd, 0 otherwise.
--
carry :: Integer -> Integer -> Integer
carry m n =
   if((mod m 2) == 1) && ((mod n 2) == 1) then 1
      else 0


-- Testing carry:
--
test_carry1 = (carry 40 5) == 0
test_carry2 = (carry 9 13) == 1
test_carry3 = (carry 20 20) == 0
test_carry4 = (carry 13 9) == 1
test_carry = test_carry1 && test_carry2 && test_carry3 && test_carry4

{-
Stepping questions

Q3.1. Replace the underlines (_______).

   expression                 justification

   (\x -> x * (3 + x)) 2             
=> 2 * (3 + 2)                by function application
=> 2 * 5                      by arithmetic
=> 10                         by arithmetic


Q3.2.  Replace the underlines (_______).
   Assume a function double has been defined:

   double :: Integer -> Integer
   double x = x * 2

   Hint: In the first step, the argument is the expression  double.

     expression                            justification

     (\a -> \b -> a (a 3)) double 0
  => (\b -> double (double 3)) 0           by function application
  => double (double 3)                     by function application
  => double (3 * 2)                        by function application
  => double 6                              by arithmetic
  => 6 * 2                                 by function application
  => 12                                    by arithmetic
-}

{-
Q4.

  The following function is named "tower".
  Given two natural numbers k and n,

                   n
                  ___  | n + i + 1 |
    tower k n  =  | |  | --------- |
                  | |  |_    i    _|

                  i=k

  If k > n, tower k n = 1.

  This "big-Pi" notation means
  "take the product of the thing on the right for all i from k to n",
  where "the thing on the right" is
 
     | n + i + 1 |
     | --------- |
     |_    i    _|

  The |_ _| represents taking the "floor": rounding down to the nearest integer.
  Haskell has a built-in function called  div  that divides *and* takes the floor.
  For example,
                 |        |   |      |
     div 15 4  = | 15 / 4 | = | 3.75 | = 3
                 |_      _|   |_    _|
-}

tower :: Integer -> Integer -> Integer
tower k n = 
   if (k > n) then 1
      else (div (n+k+1) k) * (tower (k+1) n)

-- Testing tower:
test_tower1, test_tower2, test_tower3, test_tower4 :: Bool
test_tower1 = (tower 6 5  == 1)
test_tower2 = (tower 5 5  == 2)

test_tower3 = (tower 4 7  == div (7+4+1) 4 * div (7+5+1) 5 * div (7+6+1) 6 * div (7+7+1) 7)

test_tower4 = (tower 3 14 == 110592)

test_tower  = test_tower1 && test_tower2 && test_tower3 && test_tower4



-- Q5.1: toBinary
--
-- toBinary n  ==  natural number `n' converted to a string in base 2
--
-- For example, 16 in binary is 10000, so
--
--   toBinary 16 == "10000"
--
-- Hints:
--    1. The built-in function  show  converts an integer to a string *in decimal*.
--         It may or may not be useful here, and/or in the function toNary.
--
--    2. You can use the built-in function  ++  to concatenate strings.
--         For example, "10" ++ "0" == "100".
--
--    3. Use the div and mod functions.  div was described above.
--         mod divides and returns the remainder; for example, mod 65 2 == 1,
--         because the remainder of 65 divided by 2 is 1.

toBinary :: Integer -> [Char]

toBinary 0 = ['0']
toBinary 1 = ['1']

toBinary n = 
   if (mod n 2 == 0) then toBinary(div n 2) ++ ['0']
      else toBinary(div n 2) ++ ['1']



test_toBinary1 = (toBinary 0 == "0")
test_toBinary2 = (toBinary 1 == "1")
test_toBinary3 = (toBinary 16 == "10000")
test_toBinary4 = (toBinary 32 == "100000")
test_toBinary5 = (toBinary 64 == "1000000")
test_toBinary6 = (toBinary 65 == "1000001")
test_toBinary = test_toBinary1 && test_toBinary2
                               && test_toBinary3
                               && test_toBinary4
                               && test_toBinary5
                               && test_toBinary6

-- Q5.2: toNary
--
-- toNary base n  ==  `n' converted to a string in base `base'
--
-- Raises an error if `base' is less than 2 or greater than 10.
--
-- When `base' is 2, toNary should give the same result as toBinary.
-- That is, for all `n', (toNary 2 n) == (toBinary n).
-- 
toNary :: Integer -> Integer -> [Char]

toNary base 0 = []

toNary base n
     | base < 2 || base > 10 = error "invalid base"
     | n >= 0 = toNary base (div n base) ++ show (mod n base)

test_toNary6 = (toNary 2 65 == "1000001")
test_toNaryOctal = (toNary 8 2880249322 == "25353216752")
--                           0xabad1dea

