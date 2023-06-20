module Estrutura.Grafo (
  Grafo(..),
  destinosVertice,
  programasVertice,
  verticeTemPrograma,
  Programa,
  Vertice
) where

type Destino = String
type Programa = String
type Vertice = [(Destino, Programa)]
type Grafo = [Vertice]

destinosVertice :: Vertice -> [Destino]
destinosVertice vertice = map (\ponto -> fst ponto) vertice

programasVertice :: Vertice -> [Programa]
programasVertice vertice = map (\ponto -> snd ponto) vertice

verticeTemPrograma :: Vertice -> Programa -> Bool
verticeTemPrograma vertice arvore = arvore `elem` (programasVertice vertice)