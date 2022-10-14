{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

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
-- Date: 20 november 2021
macid :: String
macid = "Sainik19"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges

{- ---------------------------------------------------------------------------------
 - maxNodeID
 - ----------------------------------------------------------------------------------
 - Description:
 - Returns the largest NodeID
 - ----------------------------------------------------------------------------------
 - |    Input    |                                                                  |
 - | Graph ns es | Graph a                                                          |
 - ----------------------------------------------------------------------------------
 - |    Output   |                                                                  |
 - | Graph [] _  | Nothing (i.e. there are no nodes)                                |
 - | Graph ns es | Maybe NodeID (i.e. maximum of NodeID)                            |
 - ----------------------------------------------------------------------------------
 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] _) = Nothing
maxNodeID (Graph ns es) = Just (maximum (map getNodeID ns))

{- ---------------------------------------------------------------------------------
 - insertNode
 - ----------------------------------------------------------------------------------
 - Description:
 - Inserts a new Node with the given value
 - ----------------------------------------------------------------------------------
 - |       Input      |                                                             |
 - |  v (Graph ns es) | Integer , Graph a                                           |
 - ----------------------------------------------------------------------------------
 - |      Output      |                                                             |
 - | v (Graph [] [])  | Graph a (i.e. Graph is returned when there                  |
 - |                  | are no nodes , then NodeId is 0)                            |              
 - | v (Graph ns es)  | Graph a (i.e. Graph is returned with v                      |
 - |                  | inserted just after the previous largest                    |
 - |                  | NodeID)                                                     |
 - ----------------------------------------------------------------------------------
 -}
insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] []) = Graph [Node 0 v] []
insertNode v (Graph ns es) = let a = [Node newID v]
                                 maybeToInt Nothing = 0
                                 maybeToInt (Just x) = x
                                 newID = maybeToInt (maxNodeID (Graph ns es)) + 1
                              in Graph (ns ++ a) es
                                 


{- ---------------------------------------------------------------------------------
 - removeNode
 - ----------------------------------------------------------------------------------
 - Description:
 - Removes any Node with the given NodeID
 - ----------------------------------------------------------------------------------
 - |       Input       |                                                            |
 - | (nID Graph ns es) | Integer , Graph a                                          |
 - ----------------------------------------------------------------------------------
 - |       Output      |                                                            |
 - | nID (Graph ns es) | Graph nnl nel (i.e list of newnodes and list of new edges) |               
 - |        nnl        | [Node x y | Node x y <- ns , x /= nID] (i.e. excluding the |
 - |                   | nodeID of that particular node in the new list)            |
 - |        nel        | [(n1, n2) | (n1,n2) <- es , n1 /= nID && n2 /= nID] (i.e.  | 
 - |                   | excluding the realtionship of that particular node in the  |
 - |                   | new edges list                                             |
 - |                   | Graph a (i.e. graph of a with removed node)                |
 - ----------------------------------------------------------------------------------
 -}
removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph ns es) = Graph nnl nel
                               where
                                 nnl = [Node x y | Node x y <- ns , x /= nID]
                                 nel = [(n1, n2) | (n1,n2) <- es , n1 /= nID && n2 /= nID]

{- ----------------------------------------------------------------------------------
 - lookupNode
 - ----------------------------------------------------------------------------------
 - Description:                                                                     
 - Returns the Node corresponding to the given NodeID                               
 - ----------------------------------------------------------------------------------
 - |       Input       |                                                            |
 - | (nID Graph ns es) | Integer , Graph a                                          |
 - ----------------------------------------------------------------------------------
 - |       Output      |                                                            |
 - | nID (Graph ns es) | if getNodeID (head ns) == nID                              |
 - |                   | then Just (head ns)                                        |
 - |   x = tail ns     | else lookupNode nID (Graph x es)                           |               
 - |                   | Maybe Intger                                               |
 - ----------------------------------------------------------------------------------
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph [] _) = Nothing
lookupNode nID (Graph ns es) 
                          |getNodeID (head ns) == nID = Just (head ns)
                          |otherwise = lookupNode nID (Graph x es)
                          where
                             x = tail ns

{- ----------------------------------------------------------------------------------
 - insertEdge
 - ----------------------------------------------------------------------------------
 - Returns the Node corresponding to the given NodeID                               
 - ----------------------------------------------------------------------------------
 - |             Input                |                                             |
 - |       (n1,n2) (Graph ns es)      | (Integer,Integer) Graph a                   |
 - ----------------------------------------------------------------------------------
 - |            Output                |                                             |                 
 - |        _ (Graph [] _)            | Nothing                                     |                
 - |    (n1,n2) (Graph ns es)         |                                             |
 - | containsBothNodes = n1 `elem`    |                                             |
 - | (map getNodeID ns) && n2 `elem`  |                                             |
 - |       (map getNodeID ns)         | if not containsBothNodes = Nothing          |
 - | containsEdge = (n1,n2) `elem` es | if containsEdge = Just (Graph ns es)        |
 - |                                  | else = Just $ Graph ns (es ++ [(n1,n2)])    |                                            
 - ----------------------------------------------------------------------------------
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes = Nothing
  | containsEdge          = Just (Graph ns es)
  | otherwise             = Just $ Graph ns (es ++ [(n1,n2)])
  where
    containsBothNodes :: Bool
    containsBothNodes = n1 `elem` (map getNodeID ns) && n2 `elem` (map getNodeID ns)
    containsEdge :: Bool
    containsEdge = (n1,n2) `elem` es

{- ------------------------------------------------------------------------

Function: maxNodeID
Test Case Number: 1
Input: Graph [Node 0 'A' , Node 1 'B' , Node 2 'C'] [(0,1),(1,2),(2,0)]
Expected Output: Just 2
Actual Output: Just 2

Function: maxNodeID
Test Case Number: 2 
Input: Graph [] []
Expected Output: Nothing
Actual Output: Nothing

Function: maxNodeID
Test Case Number: 3
Input: Graph [Node 0 'A' , Node 1 'B' , Node 2 'C', Node 11 'D'] [(0,1),(1,2),(2,0)]
Expected Output: Just 11
Actual Output: Just 11

Function: insertNode
Test Case Number: 1 
Input: 'C' (Graph [Node 11 'A', Node 30 'B'] [(11,11),(11,30)])
Expected Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 31, getNodeVal = 'C'}], 
                                    getEdges = [(11,11),(11,30)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                  Node {getNodeID = 30, getNodeVal = 'B'},
                                  Node {getNodeID = 31, getNodeVal = 'C'}], 
                                  getEdges = [(11,11),(11,30)]}

Function: insertNode
Test Case Number: 2 
Input: 'A' (Graph [] [])
Expected Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'}], getEdges = []}
Actual Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'}], getEdges = []}

Function: insertNode
Test Case Number: 3 
Input: 'C' (Graph [Node 11 'A', Node 30 'B',Node 111 'D'] [(11,11),(11,30)])
Expected Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 111, getNodeVal = 'D'},
                                    Node {getNodeID = 112, getNodeVal = 'C'}], 
                                    getEdges = [(11,11),(11,30)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                  Node {getNodeID = 30, getNodeVal = 'B'},
                                  Node {getNodeID = 111, getNodeVal = 'D'},
                                  Node {getNodeID = 112, getNodeVal = 'C'}], 
                                  getEdges = [(11,11),(11,30)]}

Function: removeNode 
Test Case Number: 1
Input: 12 (Graph [Node 11 'A', Node 30 'B',Node 12 'C',Node 111 'D'] [(11,11),(11,30)])
Expected Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 111, getNodeVal = 'D'}],
                                    getEdges = [(11,11),(11,30)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                  Node {getNodeID = 30, getNodeVal = 'B'},
                                  Node {getNodeID = 111, getNodeVal = 'D'}], 
                                  getEdges = [(11,11),(11,30)]}

Function: removeNode 
Test Case Number: 2
Input:13 (Graph [Node 11 'A', Node 30 'B',Node 12 'C',Node 111 'D'] [(11,11),(11,30),(12,30)])
Expected Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 12, getNodeVal = 'C'},
                                    Node {getNodeID = 111, getNodeVal = 'D'}], 
                                    getEdges = [(11,11),(11,30)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                  Node {getNodeID = 30, getNodeVal = 'B'},
                                  Node {getNodeID = 12,getNodeVal = 'C'},
                                  Node {getNodeID = 111, getNodeVal = 'D'}], 
                                  getEdges = [(11,11),(11,30)]}

Function: removeNode 
Test Case Number: 3
Input: 14 (Graph [Node 11 'A', Node 30 'B',Node 12 'C',Node 111 'D',Node 14 'E'] 
                  [(11,11),(11,30),(12,30),(14,12)])
Expected Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 12, getNodeVal = 'C'},
                                    Node {getNodeID = 111, getNodeVal = 'D'}], 
                                    getEdges = [(11,11),(11,30),(12,30)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 11, getNodeVal = 'A'},
                                    Node {getNodeID = 30, getNodeVal = 'B'},
                                    Node {getNodeID = 12, getNodeVal = 'C'},
                                    Node {getNodeID = 111, getNodeVal = 'D'}], 
                                    getEdges = [(11,11),(11,30),(12,30)]}

Function: lookupNode 
Test Case Number: 1
Input: 12 (Graph [] [(11,11),(11,30)])
Expected Output: Nothing
Actual Output: Nothing

Function: lookupNode 
Test Case Number: 2 
Input: 12(Graph [Node 11 'A', Node 30 'B',Node 12 'C',Node 111 'D',Node 14 'E']
       [(11,11),(11,30),(12,30),(14,12)])
Expected Output: Just (Node {getNodeID = 12, getNodeVal = 'C'})
Actual Output: Just (Node {getNodeID = 12, getNodeVal = 'C'})

Function: lookupNode 
Test Case Number: 3
Input: 120(Graph [Node 11 'A', Node 30 'B',Node 12 'C',Node 111 'D',Node 14 'E'][(11,11),(11,30),(12,30),(14,12)])
Expected Output: Nothing
Actual Output: Noyhing

Function: insertEdge
Test Case Number: 1
Input: (1,2) (Graph [Node 0 'A' , Node 1 'B' , Node 2 'C'] [(0,1),(1,2),(2,0)])
Expected Output: Just (Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'},
                       Node {getNodeID = 1, getNodeVal = 'B'},
                       Node {getNodeID = 2, getNodeVal = 'C'}],
                       getEdges = [(0,1),(1,2),(2,0)]})
Actual Output: Just (Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'},
                       Node {getNodeID = 1, getNodeVal = 'B'},
                       Node {getNodeID = 2, getNodeVal = 'C'}],
                       getEdges = [(0,1),(1,2),(2,0)]})

Function: insertEdge
Test Case Number: 2
Input: insertEdge (11,22) (Graph [Node 0 'A' , Node 1 'B' , Node 2 'C'] [(0,1),(1,2),(2,0)])
Expected Output: Nothing
Actual Output: Nothing

Function: insertEdge
Test Case Number: 3
Input: (0,2) (Graph [Node 0 'A' , Node 1 'B' , Node 2 'C'] [(0,1),(1,2),(2,0)])
Expected Output: Just (Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'},
                 Node {getNodeID = 1, getNodeVal = 'B'},
                 Node {getNodeID = 2, getNodeVal = 'C'}], 
                 getEdges = [(0,1),(1,2),(2,0),(0,2)]})
Actual Output: Just (Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'A'},
                 Node {getNodeID = 1, getNodeVal = 'B'},
                 Node {getNodeID = 2, getNodeVal = 'C'}], 
                 getEdges = [(0,1),(1,2),(2,0),(0,2)]})


-}