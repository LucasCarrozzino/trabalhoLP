import Data.List
import System.IO
import Control.Monad

andOp exp1 exp2 = zipWith (&&) exp1 exp2

orOp exp1 exp2 = zipWith (||) exp1 exp2

notOp exp1 =  [not x | x <- exp1]

implyOp exp1 exp2 = orOp (notOp exp1) exp2

equivOp exp1 exp2 = orOp (andOp exp1 exp2) (andOp (notOp exp1) (notOp exp2))

entry =""                        -- Entrada

normExp :: [Char] -> [[Char]]                              -- Normaliza a String
normExp exp
    | null exp = []
    | head exp == '(' = normExp (tail exp) 
    | head exp == ')' = normExp (tail exp)
    | head exp == ',' = "," : normExp (tail exp)  
    | head exp == '&' = "&" : (normExp (tail (tail exp))) 
    | head exp == '|' = "|" : (normExp (tail (tail exp)))
    | head exp == '-' = ">" : (normExp (tail (tail exp)))
    | head exp == '<' = "<" : (normExp (tail (tail (tail exp))))
    | head exp == '~' = "~" : normExp (tail exp)
    | otherwise = (head exp :[]) : normExp (tail exp)

normalized_exp = normExp entry


split3 :: (Eq t, Num t) => [a] -> t -> [[a]]               --Divide a lista na posição y
split3 (x:xs) y                                     
    |null xs = []
    |y == 0 = xs : []
    |otherwise = (x:[]) : (split3 xs (y+(-1)))



count   :: Eq a => a -> [a] -> Int                         --Contar numero de ocorrencias da variavel
count x =  length . filter (==x)

get_var expr                                            -- Gera lista de variaveis
    | null expr = []
    | head expr == '(' = get_var (tail expr) 
    | head expr == ')' = get_var (tail expr) 
    | head expr == '&' = get_var (tail expr)
    | head expr == ',' = get_var (tail expr) 
    | head expr == '|' = get_var (tail expr)
    | head expr == '-' = get_var (tail expr)
    | head expr == '>' = get_var (tail expr)
    | head expr == '<' = get_var (tail expr)   
    | head expr == '~' = get_var (tail expr)
    | otherwise = (head expr : []) : get_var (tail expr)

vars = nub (get_var entry)                                  -- Remove copias

expression = trim_exp normalized_exp                        -- Monta expressão mais externa, no formato [["Operação"],["operando1"],["operando2"]

trim_exp :: [[Char]] -> [[[Char]]]
trim_exp (x:xs)
     | null x = []
     | x == "&" = ("&":[]) : (op_disposition xs (ops2 xs 0))
     | x == "|" = ("|":[]) : (op_disposition xs (ops2 xs 0))
     | x == ">" = (">":[]) : (op_disposition xs (ops2 xs 0))
     | x == "<" = ("<":[]) : (op_disposition xs (ops2 xs 0))
     | x == "~" = ("~":[]) : (xs:[])
     | otherwise = [x]:[]


var_table = replicateM (length vars) [False, True]          -- Monta tabela de valores de variaveis
ver_var_table = foldr (zipWith (:)) (repeat []) var_table
generate_var_values = ver_var_table
get_var_value var = generate_var_values !! (pos var vars)

gen_expr_values :: [[[Char]]] -> [Bool]                           -- Gera Valor de um operação
gen_expr_values [] = []
gen_expr_values (x:xs)
    | xs == [] = get_var_value (head x)
    | x == ["&"] = (andOp (gen_expr_values (trim_exp (head xs))) (gen_expr_values (trim_exp (last xs))))
    | x == ["|"] = (orOp (gen_expr_values (trim_exp (head xs))) (gen_expr_values (trim_exp (last xs))))
    | x == [">"] = (implyOp (gen_expr_values (trim_exp (head xs))) (gen_expr_values (trim_exp (last xs))))
    | x == ["<"] = (equivOp (gen_expr_values (trim_exp (head xs))) (gen_expr_values (trim_exp (last xs))))
    | x == ["~"] = (notOp (gen_expr_values (trim_exp (head xs))))
    | otherwise = [False]


table :: [[[Char]]] -> [[Bool]]                            --Monta tabela de resultados para as operações
table ex                                                   
    | null ex = []
    | (length ex) == 3 = (gen_expr_values ex) : (table (trim_exp (ex !! 1)) ++ table (trim_exp (ex !! 2)))
    | (length ex) == 2 = (gen_expr_values ex) : (table (trim_exp (ex !! 1)))
    | (length ex) == 1 = []

exp_table = table expression

composed_op :: [[a]] -> [a]                --Garante que operandos sejam dispostos corretamente
composed_op (x:xs)
    | null xs = []
    | null x = []
    | length(x)==1 = x ++ composed_op xs
    | otherwise = []


op_disposition wz lin = (composed_op (split3 wz lin)):(last (split3 wz lin)):[]

table_exp_head ex                                                   
    | null ex = []
    | (length ex) == 3 = (st_ex ex) : (table_exp_head (trim_exp (ex !! 1)) ++ table_exp_head (trim_exp (ex !! 2)))
    | (length ex) == 2 = (st_ex ex) : (table_exp_head (trim_exp (ex !! 1)))
    | (length ex) == 1 = []
    | otherwise = []

st_ex ex = join(join ex)



exp_header =  reverse (table_exp_head expression)                      --Prepara cabeçalho e valores

table_header = intercalate "  |  " (vars ++ exp_header)

assemble_table = ver_var_table : (reverse exp_table) : []

table_hz = foldr (zipWith (:)) (repeat []) (join assemble_table)





bts True = " V "                                                    --Prepara para print e printa
bts False = " F "
printable table_hz = map  (map (bts)) table_hz
space tes = [ intercalate " | " x | x <-tes]

table_final =  table_header : space(printable table_hz)

print_table x = mapM_ print x
print_sat = print satisfable

--auxiliares

ops2 (x:xs) c                                                            --Encontra a virgula que separa operandos
    | null x = 0
    | x == "&" = 1 + (ops2 xs (c+1)) 
    | x == "|" = 1 + (ops2 xs (c+1))
    | x == ">" = 1 + (ops2 xs (c+1))
    | x == "<" = 1 + (ops2 xs (c+1))
    | (x == ",") && (c > 0) = 1 + (ops2 xs (c+(-1)))
    | (x == ",") && (c == 0) = 0
    | otherwise = 1 + (ops2 xs c)


pos k (x:xs)
    |k == x = 0
    |k /= x = 1 + (pos k xs)
    |otherwise = 0

test = sort (head exp_table)

satisfable                                                                 --Verifica satisfabilidade
    | (head test) == True = "A formula e uma tautologia!"
    | (last test) == True = "A formula e satisfativel"
    | (last test)== False = "A formula nao e satisfativel"





main = do 
    print_table table_final
    putStrLn " "
    print_sat