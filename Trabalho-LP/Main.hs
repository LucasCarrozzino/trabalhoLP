
import Tree
import Graph
import Checker

main = do
  --entrada
  putStrLn $ checkFramePDL [[(1, "alpha")], [(2, "gama")], [(3, "gama")]] (buildTree "Tree (Tree Empty \"alpha\" Empty) \";\" (Tree Empty \"beta\" Empty)")


