
-- Função para dividir uma string em elementos separados por um caractere
splitByChar :: Char -> String -> [String]
splitByChar _ [] = []
splitByChar delimiter str =
    let (before, rest) = break (== delimiter) str
    in before : case rest of
        [] -> []
        (_:r) -> splitByChar delimiter r

-- Função auxiliar para verificar se todas as linhas da matriz têm o mesmo tamanho
allSameLength :: [[String]] -> Bool
allSameLength [] = True
allSameLength (x:xs) = all (\row -> length row == length x) xs

-- Função para criar uma matriz a partir de uma string de entrada
createMatrixFromString :: String -> Maybe [[String]]
createMatrixFromString input = do
    let rows = splitByChar ';' input
    let matrix = createMatrix rows
    if allSameLength matrix
        then return matrix
        else Nothing

-- Função auxiliar para criar a matriz
createMatrix :: [String] -> [[String]]
createMatrix [] = []
createMatrix (row:rest) = splitByChar '|' row : createMatrix rest

-- Função para imprimir a matriz no formato de matriz
printMatrix :: [[String]] -> IO ()
printMatrix matrix = mapM_ (putStrLn . unwords) matrix

-- Função para simular as interações e imprimir o tabuleiro no estado final e 
--a quantidade de passos necessários para ele ser estabilizado
dustToDust :: Int -> Int ->  [[String]] -> Int -> IO ()--Erro: tEstavel sempre = 0
dustToDust tAtual tMax mundo tEstavel
  | tAtual <= tMax = if seeAll mundo [[""]] 0 0 == mundo then dustToDust (tAtual+1) tMax (seeAll mundo [[""]] 0 0) tEstavel else dustToDust (tAtual+1) tMax (seeAll mundo [[""]] 0 0) tAtual
  | otherwise = do
     putStrLn "A versão final do tabuleiro:"
     printMatrix mundo
     putStrLn "A quantidade de interações necessárias para o tabuleiro se estabilizar foi "
     print tEstavel

--Varre o tabuleiro e contrói a próxima interação do mesmo
seeAll :: [[String]] -> [[String]] -> Int -> Int ->  [[String]]--Erro:futuro <> [["m"]]
seeAll mundo futuro x y
  | length mundo == length futuro && length (mundo !! (length mundo - 1)) == length (futuro !! (length futuro - 1)) =  futuro
  | otherwise = if x < length mundo then seeAll  mundo (insertMat futuro (catBox mundo x y) x y) (x + 1) y else  seeAll  mundo (insertMat futuro (catBox mundo x y) x y) 0 (y + 1)

--Vê qual deve ser o próximo estado de uma célula
catBox :: [[String]] -> Int -> Int -> [[String]]
catBox mundo x y
    | currentValue == "V" && zombieNeighbors > 0 = [["Z"]]
    | currentValue == "V" && (liveNeighbors < 2 || liveNeighbors > 3) = [["0"]]
    | currentValue == "M" && liveNeighbors == 3 = [["V"]]
    | currentValue == "Z" && liveNeighbors == 0 = [["M"]]
    | otherwise = [[currentValue]]
  where
    currentValue = getValue mundo x y
    liveNeighbors = countNeighbors mundo x y "V"
    zombieNeighbors = countNeighbors mundo x y "Z"

-- Função para contar o número de células vizinhas com um valor específico
countNeighbors :: [[String]] -> Int -> Int -> String -> Int
countNeighbors mundo x y value =
    let neighbors = filter (\(i, j) -> getValue mundo i j == value) (adjacentCells x y)
    in length neighbors

-- Função para obter o valor de uma célula em determinadas coordenadas
getValue :: [[String]] -> Int -> Int -> String
getValue mundo x y =
    if x >= 0 && y >= 0 && x < length mundo && y < length (head mundo)
        then mundo !! x !! y
        else ""

--Insere a matrix no lugar certo
insertMat :: [[String]] -> [[String]] -> Int -> Int -> [[String]]
insertMat futuro cel x y
  | length futuro == 1 && length (head futuro) == 1 = cel
  | length futuro < (x + 1) = futuro <> cel
  | otherwise = futuro

main :: IO ()
main = do
    putStrLn "Informe o tabuleiro"
    input <- getLine
    case createMatrixFromString input of
        Just matrix -> do
            putStrLn "carregando tabuleiro..."
            printMatrix matrix
            putStrLn "Informe o número máximo de interações"
            limit <- getLine
            dustToDust 0 (read limit) matrix 0
        Nothing -> do
            putStrLn "Por favor, informar um tabuleiro no formato suportado"
            main

-- função ver(matriz, aux)
-- aux.tamanho ==matriz.tamanho?return aux
-- (proxima coord de aux)  = matrix[x==2?x:0][x==2?y:y+1]
