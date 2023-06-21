module Graph (
  Graph(..),
  destinosVertice,
  programsVertice,
  verticeCheckProgram,
  Program,
  Vertice
) where

type Destino = Int
type Program = String
type Vertice = [(Destino, Program)]
type Graph = [Vertice]

destinosVertice :: Vertice -> [Destino]
destinosVertice vertice = map (\ponto -> fst ponto) vertice

programsVertice :: Vertice -> [Program]
programsVertice vertice = map (\ponto -> snd ponto) vertice

verticeCheckProgram :: Vertice -> Program -> Bool
verticeCheckProgram vertice tree = tree `elem` (programsVertice vertice)
