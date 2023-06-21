
import Estrutura.Arvore
import Estrutura.Grafo
import System.IO
import VerificadordePdls

main :: IO()
main = do
  --entrada
  putStrLn $ verificaFramePDL [[(1, "alpha")], [(2, "gama")], []] (criaArvore "Arvore (Arvore Vazia \"alpha\" Vazia) \";\" (Arvore Vazia \"beta\" Vazia)")


