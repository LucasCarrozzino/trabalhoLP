module Checker
(
  checkFramePDL
) where
import Graph
import Tree
import Debug.Trace

type Frame = Graph
type PDL = Tree
type State = Int

checkFramePDL :: Frame -> PDL -> String
checkFramePDL graph tree
  | answer >= 0 = "Error.\nIn destino ---> " ++ show(answer) --show(head(filter(\aresta -> (fst aresta) == answer) (graph !! InitialState))) 
  | otherwise = "Correct."

  where
    InitialState = 0 
    answer = checkFramePdlRecursive graph tree InitialState
    

checkFramePdlRecursive :: Frame -> PDL -> State -> State
checkFramePdlRecursive graph tree destino
  | tree == Vazia = -1
  | isBase tree = if(verticeCheckProgram vertice (getSymbol tree)) then -1 else destino
  | getSymbol tree == ";" = composition (checkFramePdlRecursive graph (getLeft tree) destino) (checkFramePdlRecursive graph (getRight tree) (nextState vertice tree))
  | getSymbol tree == "U" = choiceND (checkFramePdlRecursive graph (getLeft tree) destino) (checkFramePdlRecursive graph (getRight tree ) destino) destino
  where
    vertice = graph !! destino

composition :: State -> State -> State
composition a b
  | a /= (-1) = a
  | b /= (-1) = b
  | otherwise = -1

choiceND :: State -> State -> State -> State
choiceND a b InitialState
  | a == (-1) && b == (-1) = -1
  | otherwise = InitialState

nextState :: Vertice -> PDL -> State
nextState vertice tree
  | isBase tree = fst(head(filter (\aresta -> (snd aresta) == (getSymbol tree)) vertice))
  | getSymbol tree == ";" = nextState vertice (getLeft tree)
