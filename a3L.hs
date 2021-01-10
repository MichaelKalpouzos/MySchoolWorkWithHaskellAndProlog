-- CISC 360 a3L, Fall 2020
-- Sample Solution

-- SEE THE FILE a3L.pdf
-- for instructions

module A3L
where
import Data.List

-- Q1:
-- Add your student ID (if in a group of 2, either student's ID):
student_id :: Integer
student_id = 20116078

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.

{-
Q2: Truth Tables

In order to build a truth table for a formula, there are 4 steps:

1) Traverse the formula to find all atomic propositions (propositional variables).

2) Assign all possible combinations of True and False
   to the set of atomic propositions in the formula.

3) Evaluate the formula for each valuation obtained in (2).

4) Use the results of (1-3) to build the table.

In this question, you will implement steps (1-3).
-}

-- Variable is a synonym for String.
type Variable = String

-- In our simplified version of classical propositional logic,
-- we have the following definition for a Formula:
data Formula = TOP                          -- truth
             | BOT                          -- falsehood (contradiction)
             | AND Formula Formula          -- conjunction
             | OR Formula Formula           -- disjunction
             | IMPLIES Formula Formula      -- implication
             | NOT Formula                  -- negation
             | ATOM Variable                -- atomic proposition (propositional variable)
             deriving (Eq, Show)

-- Some propositional variables, for convenience
vA = ATOM "A"
vB = ATOM "B"
vC = ATOM "C"
vD = ATOM "D"
vE = ATOM "E"
vF = ATOM "F"

-- Some example formulas that you can use to test your functions
formula1 = IMPLIES (AND vA vB) vC
formula2 = IMPLIES BOT (AND vA vB)
formula3 = IMPLIES (AND vA vB) TOP
formula4 = AND (IMPLIES vA (AND vB vC)) (AND vD vE)
formula5 = AND vA vB
formula6 = NOT vA
formula7 = IMPLIES vA vB
formula8 = OR vA (NOT vA)
formula9 = OR vA (NOT vB)

-- A Valuation is a list of pairs corresponding to a truth value (i.e. True or False) for each Variable in a formula
type Valuation = [(Variable, Bool)]

-- A TruthTable is an enumeration of the valuations for a given formula,
-- with each valuation paired with the corresponding evaluation of that formula.
-- (This corresponds to a truth table with no "intermediate columns".)
data TruthTable = TruthTable [(Valuation, Bool)]

-- This function is here so that when you print a TruthTable in GHCi, the table is nice and readable.
-- You don't need to understand how this works to complete the assignment.
instance Show TruthTable where
  show (TruthTable rows) =
    case rows of
      [] -> ""
      ([], result) : _ -> "   result is " ++ pad_show result ++ "\n"
      ((c,b) : valu, result) : xs -> 
        c ++ "=" ++ (pad_show b) ++ "   "
          ++ show (TruthTable [(valu,result)])
          ++ show (TruthTable xs)
    where
      pad_show True  = "True "
      pad_show False = "False"

{- Q2a: getAtoms:
  Traverse a formula and build a list of all ATOMs in the formula, without duplicates.
  Applied to a Formula and an (initially empty) list.
  The list parameter can be thought of as an accumulator to build up the list of ATOMs.
  Note: it may be convenient to use a built-in function called "nub",
  which removes all duplicates from a list.  Why is it called nub?  I have no idea.
-}
getAtoms :: Formula -> [Variable] -> [Variable]
getAtoms TOP               vList = vList
getAtoms BOT               vList = vList
getAtoms (ATOM v)          vList = nub(vList ++ [v])
getAtoms (NOT phi)         vList = getAtoms (phi) vList
getAtoms (AND phi1 phi2)   vList = nub(getAtoms (phi1) vList ++ getAtoms (phi2) vList)
getAtoms (OR phi1 phi2)    vList = nub(getAtoms (phi1) vList ++ getAtoms (phi2) vList)
getAtoms (IMPLIES phi psi) vList = nub(getAtoms (phi) vList ++ getAtoms (psi) vList)
-- Q2b: getValuations:
--  Build a list of all possible valuations for a set of variables
getValuations :: [Variable] -> [Valuation]
getValuations []       = [[]]
getValuations (c : cs) = (map ((c,True):) restOfTable) ++ (map ((c,False):) restOfTable) where restOfTable = getValuations cs

-- Hint: To apply a function f to every element of a list xs,
--  write  map f xs.
-- For example, the following adds 1 to the start of every list
--  in a list of lists [[2,3], [2,4]]:
--  map (\ys -> 1 : ys) [[2,3], [2,4]]  ==  [[1,2,3], [1,2,4]]


-- Q2c: evalF:
--  Evaluate a formula with a particular valuation,
--   returning the resulting boolean value
evalF :: Valuation -> Formula -> Bool
evalF _    TOP                 = True
evalF _    BOT                 = False
evalF valu (NOT phi1)          = not (evalF valu phi1)
evalF valu (ATOM c)            = boolFinder(find ((c ==) . fst) valu)
evalF valu (AND phi1 phi2)     = (evalF valu phi1) && (evalF valu phi2)
evalF valu (OR phi1 phi2)      = (evalF valu phi1) || (evalF valu phi2)
evalF valu (IMPLIES phi1 phi2) = (evalF valu phi1) == False || (evalF valu phi2) == True

-- This is a Helper function for line 126, because the evalF function can only return a boolean. This function
-- takes in [Variable, Bool] and return the boolean value that coorelates with c in ATOM c, 
-- which is the Variable in this case.
boolFinder :: Maybe (Variable, Bool) -> Bool
boolFinder (Just (variable, boolValue)) = boolValue

-- buildTable:
--  Build a truth table for a given formula.
--  You can use this function to help check your definitions
--  of getAtoms, getValuations and evalF.
buildTable :: Formula -> TruthTable
buildTable psi =
  let valuations = getValuations (getAtoms psi [])
  in
    TruthTable (zip valuations
                    (map (\valu -> evalF valu psi) valuations))

{-
Q3: Tiny Theorem Prover
-}

-- a Context is a list of Formulas, representing assumptions
type Context = [Formula]

-- prove ctx phi:
--   return True if, assuming everything in ctx is true,
--    the formula phi is true according to the rules given in a3L.pdf.
--   otherwise, return False.
prove :: Context -> Formula -> Bool
prove ctx phi =  
  if elem phi (decompose [] ctx) || elem BOT (decompose [] ctx) || prove_right (decompose [] ctx) phi
  then True
  else False

-- decompose ctx1 ctx2
--  move through ctx2, decomposing ANDs into standalone assumptions
--                     and eliminating IMPLIESes where possible
--                     (see a3L.pdf).
-- invariants:
--  - ctx1 is completely decomposed (no formula in ctx1 is (AND _ _))
--  - ctx2 is a "queue" of assumptions to decompose
decompose :: Context -> Context -> Context
decompose ctx1 []              = ctx1
decompose ctx1 (middle : ctx2) =
  case middle of
    AND phi1 phi2   -> decompose ctx1 ([phi1, phi2] ++ ctx2) 
    IMPLIES phi psi -> 
      if (prove (ctx1 ++ ctx2) phi) 
      then decompose ctx1 ([psi] ++ ctx2)
      else decompose (ctx1 ++ [middle]) ctx2

    middle          -> decompose (ctx1 ++ [middle]) ctx2

-- decompose (ctx1 ++ [phi2,phi1]) ctx2 

-- `case' does pattern matching without declaring a separate function
 

-- prove_right:
--  assuming the context is decomposed,
--  apply -Right rules (see a3L.pdf)
--   to break down the goal formula
--  ("right" because we are working on the formula on the right-hand side,
--   after the assumptions)
prove_right :: Context -> Formula -> Bool

prove_right ctx TOP               = True     -- TOP-Right

prove_right ctx (AND phi1 phi2)   = 
  if ((prove ctx phi1) && (prove ctx phi2))
  then True
  else False
  -- try to apply AND-Right

prove_right ctx (OR phi1 phi2)   = 
  if ((prove ctx phi1) || (prove ctx phi2))
  then True
  else False
  -- try to apply OR-Right-1; if that fails, try OR-Right-2

prove_right ctx (IMPLIES phi psi) = (prove (phi : ctx) psi)
  -- try to apply IMPLIES-Right

prove_right ctx p             =
  -- couldn't apply any of the -Right rules, so give up
  False

test_imp1 = prove [IMPLIES vB vC] (IMPLIES vB vC)
test_imp2 = prove [IMPLIES vB vC] (IMPLIES (AND vB vB) vC)
test_imp3 = not (prove [IMPLIES (AND vB vD) vC] (IMPLIES vB vC))





{-
   BONUS (up to 5% marks; if you get 100% on the main assignment and do
     the bonus perfectly, you will get 105%, but this probably requires
     much more than 5% of the effort of the main assignment)

   Implement the following rule:
 
     ctx1 ++ [phi1] ++ ctx2 |- psi        ctx1 ++ [phi1] ++ ctx2 |- psi
     ------------------------------------------------------------------
                 ctx1 ++ [OR phi1 phi2] ++ ctx2 |- psi

   You probably need to restructure  prove, decompose,  and  prove_right,
   so begin by copying your solutions to  prove, decompose and prove_right below,
   renaming them to prov2, decompose2 and prove_right2.
   Do NOT change the types of prove, decompose or prove_right;
   the main questions are marked separately.
   Also, do not change the type of prove2; it should be possible to call prove2
   in the same way as we call 'prove'.
   But you may change the types of decompose2 and prove_right2, add new helper functions,
   or possibly remove those functions and solve the problem in some different way
   (as long as you implement the above rule).
-}
