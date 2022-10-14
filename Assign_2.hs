{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where

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
-- Date: 29 october 2021
macid :: String
macid = "Sainik19"

type Vector3D = (Double,Double,Double)

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   Returns x coordinate
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |   (x,y,z)   | (Double,Double,Double)                          |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |     (a,b,c) | a                                               |
 - -----------------------------------------------------------------
 -}
getX :: Vector3D -> Double
getX (a,b,c) = a

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -   Returns y coordinate
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |   (x,y,z)   | (Double,Double,Double)                          |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |     (a,b,c) | b                                               |
 - -----------------------------------------------------------------
 -}
getY :: Vector3D -> Double
getY (a,b,c) = b

{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -   Returns z coordinates
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |   (x,y,z)   | (Double,Double,Double)                          |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |     (a,b,c) | c                                               |
 - ------------------------------------------------------------------
 -}
getZ :: Vector3D -> Double
getZ (a,b,c) = c

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 -   Performs scalar mutiplication
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |    s , v    | Double input , (Double,Double,Double)           |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |    s , v    | s * getX = a                                    |
 - |    s , v1   | s * getY = b                                    |
 - |    s , v2   | s * getZ = c                                    |  
 - |             | (a,b,c)                                         |
 - ------------------------------------------------------------------
 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s v = (s * (getX v), s* (getY v), s* (getZ v))

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -   Perorms 3D vector addition
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |    v0 , v1  | (Double,Double,Double) , (Double,Double,Double) |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |    v0 , v1  | getX v0 + getX v1 = a                           |
 - |    v0 , v1  | getY v0 + getY v1 = b                           |
 - |    v0 , v1  | getZ v0 + getZ v1 = c                           |  
 - |             | (a,b,c)                                         |
 - ------------------------------------------------------------------
 -}
add :: Vector3D -> Vector3D -> Vector3D
add v0 v1 = (getX v0 + getX v1, getY v0 + getY v1, getZ v0 + getZ v1)


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 - Implements the inner product operation for a 3D vector space
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |    v0 , v1  | (Double,Double,Double) , (Double,Double,Double) |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |    v0 , v1  | getX v0 * getX v1 = a                           |
 - |    v0 , v1  | getY v0 * getY v1 = b                           |
 - |    v0 , v1  | getZ v0 * getZ v1 = c                           |  
 - |   a , b ,c  | a + b + c = d                                   |
 - |             | d Double output                                 |
 - ------------------------------------------------------------------
 -}
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct v0 v1 = getX v0 * getX v1 + getY v0 * getY v1 + getZ v0 * getZ v1

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 - Gives the vector distance between the elements
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |    v1 , v2  | (Double,Double,Double) , (Double,Double,Double) |
 - -----------------------------------------------------------------
 - |    Output   |                                                 |
 - |    v1 , v2  | (getX v1 * getX v2) ^ 2 = a                     |
 - |    v1 , v2  | (getY v1 * getY v2) ^ 2 = b                     |
 - |    v1 , v2  | (getZ v1 * getZ v2) ^ 2 = c                     |  
 - |   a , b ,c  | a + b + c = d                                   |
 - |      d      | sqrt d = e                                      |
 - |             | e Double output                                 |
 - -----------------------------------------------------------------
 -}
distance :: Vector3D -> Vector3D -> Double
distance v1 v2 = sqrt (((getX v1 - getX v2) ^ 2) + ((getY v1 - getY v2) ^ 2) + ((getZ v1 - getZ v2) ^ 2)) 

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -  Returns the vector with maximum distance from(0,0,0)
  - --------------------------------------------------------------------------------
 - |    Input    |                                                                 |
 - |     vs      |[(Double,Double,Double),(Double,Double,Double)...]               |
 - ---------------------------------------------------------------------------------
 - |    Output   |                                                                 |
 - | if list = []| (0,0,0) output (i.e. list is empty)                             |
 - |if list = [a | (a1,a2,a3) output (i.e. list have single element)               |
 - |   (v:y:vs)  | if  distance v (0,0,0) > distance y (0,0,0) = maxDistance (v:vs)|
 - |             | else axDistance (y:vs)                                          |                  
 - |             | Tuple (Double,Double,Double)output                              | 
 - ---------------------------------------------------------------------------------
 -}

maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0,0,0)
maxDistance [a] = a
maxDistance (v:y:vs) 
                  | distance v (0,0,0) > distance y (0,0,0) = maxDistance (v:vs)
                  | otherwise = maxDistance (y:vs)

{- ------------------------------------------------------------------------

Function: scalarMult
Test Case Number: 1
Input: 2 (2,4,6)
Expected Output: (4.0,8.0,12.0)
Actual Output: (4.0,8.0,12.0)

Function: scalarMult
Test Case Number: 2 
Input: 2 (22/7,2,3)
Expected Output: (6.28571428571,4.0,6.0)
Actual Output: (6.285714285714286,4.0,6.0)

Function: scalarMult
Test Case Number: 3
Input: 2.2 (1,2,3)
Expected Output: (2.2,4.4,6.6)
Actual Output: (2.2,4.4,6.6000000000000005)

Function: add
Test Case Number: 1 
Input: (3,8,44) (5,11,34.654)
Expected Output: (8.0,19.0,78.654)
Actual Output: (8.0,19.0,78.654)

Function: add
Test Case Number: 2 
Input: (99,101.76427,223) (22,44,65)
Expected Output: (121.0,145.76427,288.0)
Actual Output: (121.0,145.76427,288.0)

Function: add
Test Case Number: 3 
Input: (33/11,2,3) (2,44/3,2)
Expected Output: (5.0,16.66666667,5.0)
Actual Output: (5.0,16.666666666666664,5.0)

Function: innerProduct
Test Case Number: 1
Input: (2.2,5,7) (31.44,4,8)
Expected Output: 145.168
Actual Output: 145.168

Function: innerProduct
Test Case Number: 2
Input: (9,8,2) (5,3,90)
Expected Output: 249.0
Actual Output: 249.0

Function: innerProduct
Test Case Number: 3 
Input: (50,60,10) (7,3,4)
Expected Output: 570.0
Actual Output: 570.0

Function: distance
Test Case Number: 1
Input: (4,6,3) (5,2,6)
Expected Output: 5.0990195135928
Actual Output: 5.0990195135927845

Function: distance
Test Case Number: 2 
Input: (3.21,6.666,7.88) (2,4.567,44)
Expected Output: 36.201164359727 
Actual Output: 36.201164359727436

Function: distance
Test Case Number: 3
Input: (2.2,10,50) (40,5,6)
Expected Output: 58.222332485052
Actual Output: 58.2223324850525

Function: maxDistance
Test Case Number: 1
Input: [(1,2,3),(0,0,0),(22/7,11.5,2)]
Expected Output: (22/7,11.5,2)
Actual Output: (3.142857142857143,11.5,2.0)

Function: maxDistance
Test Case Number: 2
Input: [(1,2,3),(9,2,0.1),(1,2,3),(8,6,44)]
Expected Output: (8,6,44)
Actual Output: (8.0,6.0,44.0)

Function: maxDistance
Test Case Number: 3
Input: [(1,2,3),(9,2,0.1),(1,2,3),(8,6,44),(2,2,2),(8,6,44),(6.000001,55.222222,4.56)]
Expected Output: (6.000001,55.222222,4.56)
Actual Output: (6.000001,55.222222,4.56)


-}