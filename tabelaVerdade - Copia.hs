import Data.List
import System.IO
import Control.Monad


normExp :: [Char] -> [[Char]]                              -- Normaliza a String
normExp exp
    | null exp = []
    | head exp == '(' = normExp (tail exp)
    | head exp == ')' = normExp (tail exp)
    | head exp == ';' = ";" : (normExp (tail (tail exp)))
    | head exp == '?' = "?" : (normExp (tail (tail exp)))
    | head exp == '*' = "*" : (normExp (tail (tail exp)))
    | head exp == '>' = ">" : (normExp (tail (tail exp)))
    | head exp == '<' = "<" : (normExp (tail (tail exp)))
    | head exp == '[' = "[" : (normExp (tail (tail exp)))
    | head exp == ']' = "]" : (normExp (tail (tail exp)))
    | head exp == 'v' = "v" : (normExp (tail (tail exp)))
    | head exp == '-' = "-" : (normExp (tail (tail exp)))
    | otherwise = (head exp :[]) : normExp (tail exp)



main = do
    proposicao <- getLine
    programas <- getLine
    --let norm/grafo = normPro programas
    let norm = normExp proposicao
    print norm
