module Estrutura.Arvore
(
  Arvore(..),
  getSimbolo,
  getEsquerda,
  getDireita,
  isFolha,
  criaArvore
) where

data Arvore = Vazia 
  | Arvore Arvore String Arvore 
  deriving (Show, Read, Eq)

isFolha :: Arvore -> Bool
isFolha arvore = getEsquerda arvore == Vazia && getDireita arvore == Vazia

getEsquerda :: Arvore -> Arvore
getEsquerda (Arvore getEsquerda _ _) = getEsquerda

getDireita :: Arvore -> Arvore
getDireita (Arvore _ _ getDireita) = getDireita

getSimbolo :: Arvore -> String
getSimbolo (Arvore _ getSimbolo _) = getSimbolo

criaArvore :: String -> Arvore
criaArvore str = read str :: Arvore