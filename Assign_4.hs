{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where

import Test.QuickCheck

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Khushii Saini
-- Date: 10th December 2021
macid :: String
macid = "Sainik19"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int
  deriving (Show,Eq,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description: e is an expression and v is a floating point number,
 -              it evaluates the value of e at v
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |     e v     | MathExpr a, Float                               |
 - |   Example   | (Func1 (Power (-2)) X), 2                       |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |     e v     | (Func1 (Power (-2)) 2)                          |
 - |             | 0.25                                            |
 - -----------------------------------------------------------------
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef c) v = c
eval (Func1 op e) v =
  case op of
    Power n -> (eval e v) ^^ n
    Cos     -> cos (eval e v)
    Sin     -> sin (eval e v)
    Abs     -> abs (eval e v)
eval (Func2 op e0 e1) v =
  case op of
    Add  -> eval e0 v + eval e1 v
    Mult -> eval e0 v * eval e1 v
{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description: It defines the implementations of addition, 
 -              multiplication, negation, absolute value and
 -              conversion to integer
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult x (Coef (-1)) 
  abs x         = Func1 Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description: It defines the implementations of reciproal and 
 -              conversion to rational number
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power(-1)) e
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description: It defines the implementations of pi, sinx and cosx
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin y   = Func1 Sin y
  cos y   = Func1 Cos y
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: It symbolically differentiate using differentiation 
                rules
 - -----------------------------------------------------------------
 - |        Input      |                                           |
 - |          u        | MathExpr a                                | 
 - |       Example     | Func2 Mult(X)(65)                         |
 - -----------------------------------------------------------------
 - |      Output       |                                           |
 - | Func2 Mult(X)(65) | Func2 Add (Func2 Mult (Coef 1.0)          |
 - |                   | (Coef 65.0))(Func2 Mult X (Coef 0.0))     |
 - |                   | MathExpr a                                |
 - -----------------------------------------------------------------
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
diff (Coef a) = Coef 0
diff(Func2 Add x y) = diff x + diff y
diff(Func2 Mult x y) = diff x *y + x * diff y
diff (Func1 (Power n) x) = fromInteger(fromIntegral n) * Func1 (Power (n-1)) x * diff x
diff (Func1 Cos x) = -sin x * diff x
diff (Func1 Sin x) = cos x * diff x
diff (Func1 Abs x) = (x/ abs x) * diff x

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description: Cretes string representation of mathematical 
 -              expression
  - -----------------------------------------------------------------
 - |      Input       |                                             |
 - |        u         | MathExpr a                                  | 
 - |     Example      | Func2 Mult(X)(65)                           |
 - ------------------------------------------------------------------
 - |      Output      |                                             |
 - | Func2 Mult(X)(65)| "(X * 65)" (String)                         |
 - ------------------------------------------------------------------
 -}
pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef a) = show a
pretty(Func2 Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
pretty(Func2 Mult x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
pretty (Func1 (Power n) x) = "(" ++ pretty x ++ " ^^ " ++ show n ++ ")"
pretty (Func1 Cos x) = "cos(" ++ pretty x ++ ")"
pretty (Func1 Sin x) = "sin(" ++ pretty x ++ ")"
pretty(Func1 Abs x) = "abs(" ++ pretty x ++ ")"

{- -----------------------------------------------------------------

Function: eval
Test Case Number: 1
Input: eval  (Func1 (Power (-2)) X) 2
Expected Output: 0.25
Actual Output: 0.25

Function: eval
Test Case Number: 2
Input: eval  (Func2 Add(X)(4)) 5.67890
Expected Output: 9.67890
Actual Output: 9.678899999999999

Function: eval
Test Case Number: 3
Input: eval  (Func2 Mult(X)(4.5431)) 11.3490
Expected Output: 51.5596419
Actual Output: 51.5596419

Function: diff
Test Case Number: 1
Input: diff  (Func2 Mult(X)(65)) 
Expected Output: Func2 Add (Func2 Mult (Coef 1.0) (Coef 65.0)) (Func2 Mult X (Coef 0.0))
Actual Output: Func2 Add (Func2 Mult (Coef 1.0) (Coef 65.0)) (Func2 Mult X (Coef 0.0))

Function: diff
Test Case Number: 2
Input: diff (Func2 Add (Func2 Mult (Coef 1.0) (Coef 65.0)) (Func2 Mult X (Coef 0.0)))
Expected Output: Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) (Coef 65.0)) 
                (Func2 Mult (Coef 1.0) (Coef 0.0))) 
                (Func2 Add (Func2 Mult (Coef 1.0) (Coef 0.0)) (Func2 Mult X (Coef 0.0)))
Actual Output: Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) (Coef 65.0)) 
                (Func2 Mult (Coef 1.0) (Coef 0.0))) 
                (Func2 Add (Func2 Mult (Coef 1.0) (Coef 0.0)) (Func2 Mult X (Coef 0.0)))

Function: diff
Test Case Number: 3
Input: diff ( Func2 Add (Func2 Mult (X) (9)) (Func1 (Power (-2)) X))
Expected Output: Func2 Add (Func2 Add (Func2 Mult (Coef 1.0) (Coef 9.0)) (Func2 Mult X (Coef 0.0))) 
                 (Func2 Mult (Func2 Mult (Coef (-2.0)) (Func1 (Power (-3)) X)) (Coef 1.0))
Actual Output: Func2 Add (Func2 Add (Func2 Mult (Coef 1.0) (Coef 9.0)) (Func2 Mult X (Coef 0.0)))
               (Func2 Mult (Func2 Mult (Coef (-2.0)) (Func1 (Power (-3)) X)) (Coef 1.0))

Function: pretty
Test Case Number: 1
Input: pretty (Func2 Add (Func2 Mult (Coef 1.0) (Coef 65.0)) (Func2 Mult X (Coef 0.0)))
Expected Output: "((1.0 * 65.0) + (X * 0.0))"
Actual Output: "((1.0 * 65.0) + (X * 0.0))"

Function: pretty
Test Case Number: 2
Input: pretty (Func2 Add (Func2 Add (Func2 Mult (Coef 0.0) (Coef 65.0)) 
      (Func2 Mult (Coef 1.0) (Coef 0.0))) (Func2 Add (Func2 Mult (Coef 1.0)
      (Coef 0.0)) (Func2 Mult X (Coef 0.0))))
Expected Output: "(((0.0 * 65.0) + (1.0 * 0.0)) + ((1.0 * 0.0) + (X * 0.0)))"
Actual Output: "(((0.0 * 65.0) + (1.0 * 0.0)) + ((1.0 * 0.0) + (X * 0.0)))"

Function: pretty
Test Case Number: 3
Input: pretty(Func2 Add (Func2 Add (Func2 Mult (Coef 1.0) (Coef 9.0)) (Func2 Mult X (Coef 0.0)))
      (Func2 Mult (Func2 Mult (Coef (-2.0)) (Func1 (Power (-3)) X)) (Coef 1.0)))
Expected Output: "(((1.0 * 9.0) + (X * 0.0)) + ((-2.0 * (X ^^ -3)) * 1.0))"
Actual Output: "(((1.0 * 9.0) + (X * 0.0)) + ((-2.0 * (X ^^ -3)) * 1.0))"

-}
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Func2 Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

{- Function: eval
 - Property: eval (Func2 Mult (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y) = (x * y) =~ eval (Func2 Mult (Coef x) X) y

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck  evalProp1


