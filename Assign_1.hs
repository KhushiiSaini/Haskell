{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2021
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

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
macid :: String
macid = "Sainik19"



{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- ------------------------------------------------------------------
 - sinTaylor
 - ------------------------------------------------------------------
 - Description:
 -   Computes the 4th Taylor Polynomial Approximation
 - ------------------------------------------------------------------
 - |        Input         |                                         |
 - | a, cos(a), sin(a), x | Double input                            |
 - ------------------------------------------------------------------
 - |        Output        |                                         |
 - |                      | (sin(a)/0) * (0-a) ** 0 = p             |
 - |          p           | p + ((cos(a)/1) * (1-a)) ** 1 = q       |
 - |          q           | q + ((-sin(a)/1*2) * (2-a)) ** 2 = r    |
 - |          r           | r + ((-cos(a)/1*2*3) * (3-a)) ** 3 = s  |
 - |          s           | s + ((sin(a)/1*2*3*4) * (4-a)) ** 4     |
- -------------------------------------------------------------------
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x = ((sin_a / fromIntegral (factorial 0)) * ((x - a) ** 0))
                            + ((cos_a / fromIntegral (factorial 1)) * ((x - a) ** 1))
                            + ((- sin_a / fromIntegral (factorial 2)) * ((x- a) ** 2))
                            + ((- cos_a / fromIntegral (factorial 3)) * ((x - a) ** 3))
                            + ((sin_a / fromIntegral (factorial 4)) * ((x - a) ** 4))

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description:
 -   Computes the mod of two double x and y
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |    x,y      | Double input                                    |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |     z       | x/y                                             |
 - |     x/y     | floor(x / y)                                    |
 - | floor(x/y)  | fromIntegral(floor(x/y))                        |
 - |             | x - z * y                                       |
 - -----------------------------------------------------------------
 -}
fmod :: Double -> Double -> Double
fmod x y =
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z = fromIntegral (floor (x / y))
    in x - z*y

{- ----------------------------------------------------------------------
 - sinApprox
 - ----------------------------------------------------------------------
 - Description:
 -   Computes an approximation of sin(x) using sinTaylor
 - ----------------------------------------------------------------------
 - |    Input    | 
 - |      x      | Double Integer
 - ----------------------------------------------------------------------
 - |        Output           |                                                                                            |
 - |     fmod x (2*pi)       | y                                                                                          |
 - |    if 0 <= y < pi/4     | (0/0) * (0-0) ** 0 + (1/1) * (1-0)) ** 1 + .... + (0/1*2*3*4) * (4-0)) ** 4                |
 - |  if pi/4 <= y < 3*pi/4  | (1/0) * (0-pi/2) ** 0 + (0/1) * (1-pi/2)) ** 1 + ... + (1/1*2*3*4) * (4-pi/2)) ** 4        |
 - | if 3*pi/4 <= y < 5*pi/4 | (-1/0) * (0-pi) ** 0 + (-1/1) * (1-pi)) ** 1 + ... + (0/1*2*3*4) * (4-pi)) ** 4            |
 - | if 5*pi/4 <= y < 7*pi/4 | (0/0) * (0-3*pi/2) ** 0 + (0/1) * (1-3*pi/2)) ** 1 + ... + (-1/1*2*3*4) * (4-3*pi/2)) ** 4 |
 - |          else           | (0/0) * (0-2*pi) ** 0 + (1/1) * (1-2*pi)) ** 1 + ... + (0/1*2*3*4) * (4-2*pi)) ** 4        |
 - |                         | sin(x)                                                                                     |
 - --------------------------------------------------------------------------------------
 -}
sinApprox :: Double -> Double
sinApprox x =
  let
    z = fmod x (2*pi)
  in 
    if z >= 0 && z < pi/4
      then sinTaylor 0 1 0 z
      else
        if z >= pi/4 && z < 3*pi/4
        then sinTaylor (pi/2) 0 1 z
        else
          if z >= 3*pi/4 && z < 5*pi/4
            then sinTaylor pi (-1) 0 z
            else
              if z >= 5*pi/4  && z < 7*pi/4
              then sinTaylor (3*pi/2) 0 (-1) z
              else
                 sinTaylor (2*pi) 1 0 z

{- ---------------------------------------------------------------------
 - sinApprox
 - ---------------------------------------------------------------------
 - Description:
 -   Computes an approximation of cos(x) using sinApprox
 - ---------------------------------------------------------------------
 - |    Input    |                                                     |
 - |      x      | Double input                                        |
 - ---------------------------------------------------------------------
 - |    Output   |                                                     |
 - |     angle   | -x + (pi/2)                                         |
 - | -x + (pi/2) | sinApprox (-x +(pi/2))                              |
 - |             | - sin(x - pi/2)                                     |
 - ---------------------------------------------------------------------
 -}
cosApprox :: Double -> Double
cosApprox x = let angle = - x + (pi/2)
              in sinApprox angle

{- ---------------------------------------------------------------------
 - tanApprox
 - ---------------------------------------------------------------------
 - Description:
 -   Computes and approximation of tan(x) using sinApprox and cosApprox
 - ---------------------------------------------------------------------
 - |    Input   |                                                      |
 - |      x     | Double input                                         |
 - ---------------------------------------------------------------------
 - |    Output   |                                                     |
 - | sinApprox x | sin(x)                                              |
 - | cosApprox x | cos(x)                                              |
 - |             | sin(x)/cos(x)                                       |
 - ---------------------------------------------------------------------
 -}
tanApprox :: Double -> Double
tanApprox x = sinApprox x / cosApprox x 
