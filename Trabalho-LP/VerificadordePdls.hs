module VerificadordePdls
(
  verificaFramePDL
) where
import Estrutura.Arvore
import Estrutura.Grafo
import Debug.Trace

type Frame = Grafo
type PDL = Arvore
type Estado = Int

verificaFramePDL :: Frame -> PDL -> String
verificaFramePDL grafo arvore
  | resultado == (-1) = "Correto." 
  | otherwise = "Erro.\nNo destino ---> " ++ show(resultado) --show(head(filter(\aresta -> (fst aresta) == resultado) (grafo !! primeiroEstado)))

  where
    primeiroEstado = 0 
    resultado = verificaFramePdlRecursivo grafo arvore primeiroEstado
    

verificaFramePdlRecursivo :: Frame -> PDL -> Estado -> Estado
verificaFramePdlRecursivo grafo arvore destino
  | arvore == Vazia = -1
  | isFolha arvore = if(verticeTemPrograma vertice (getSimbolo arvore)) then -1 else destino
  | getSimbolo arvore == "V" = decisaoDeV (verificaFramePdlRecursivo grafo (getEsquerda arvore) destino) (verificaFramePdlRecursivo grafo (getDireita arvore ) destino) destino
  | getSimbolo arvore == ";" = sequenciaDePonto (verificaFramePdlRecursivo grafo (getEsquerda arvore) destino) (verificaFramePdlRecursivo grafo (getDireita arvore) (estadoSeguinte vertice arvore))
  where
    vertice = grafo !! destino

decisaoDeV :: Estado -> Estado -> Estado -> Estado
decisaoDeV a b estadoInicial
  | a == (-1) && b == (-1) = -1
  | otherwise = estadoInicial
  
sequenciaDePonto :: Estado -> Estado -> Estado
sequenciaDePonto a b
  | a /= (-1) = a
  | b /= (-1) = b
  | otherwise = -1

estadoSeguinte :: Vertice -> PDL -> Estado
estadoSeguinte vertice arvore
  | isFolha arvore = fst(head(filter (\aresta -> (snd aresta) == (getSimbolo arvore)) vertice))
  | getSimbolo arvore == ";" = estadoSeguinte vertice (getEsquerda arvore)
