
import Estrutura.Arvore
import Estrutura.Grafo
import VerificadordePdls

main = do
  putStrLn $ "Corretos:"
  
  putStrLn $ "alpha; beta"
  putStrLn $ verificaFramePDL [[(0, "alpha"), (1, "beta")]] (criaArvore "Arvore (Arvore Vazia \"alpha\" Vazia) \";\" (Arvore Vazia \"beta\" Vazia)") 
  putStrLn $ "\n"
 
  putStrLn $ "alpha V beta"
  putStrLn $ verificaFramePDL [[(0, "alpha"), (1, "beta")]] (criaArvore "Arvore (Arvore Vazia \"alpha\" Vazia) \"V\" (Arvore Vazia \"beta\" Vazia)") 
  putStrLn $ "\n"

  putStrLn $ "Errados:"
  putStrLn $ "\n"

  putStrLn $ "a;b"
  putStrLn $ verificaFramePDL [[(1, "alpha")], [(2, "gama")], []] (criaArvore "Arvore (Arvore Vazia \"alpha\" Vazia) \";\" (Arvore Vazia \"beta\" Vazia)") 
  putStrLn $ "\n"

  putStrLn $ "b;a"
  putStrLn $ verificaFramePDL [[(1,"a"), (2, "b"), (3, "a")], [(1,"a")], [], []] (criaArvore "Arvore (Arvore Vazia \"b\" Vazia) \";\" (Arvore Vazia \"a\" Vazia)") 
  putStrLn $ "----------------------------------"
  putStrLn $ "\n"
  
  