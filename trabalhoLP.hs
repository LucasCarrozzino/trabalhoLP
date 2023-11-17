import Data.List

-- Função principal
main :: IO ()
main = do
    putStrLn "Informe a grade no formato 'V|M|0;M|M|0;V|M|V;' (exemplo: 'M|V|M;V|0|M;0|V|0;'): "
    input <- getLine
    case createMatrixFromString input of
        Just matrix -> do
            putStrLn "Grade inicial:"
            printMatrix matrix
            putStrLn "Informe o número máximo de iterações: "
            limit <- getLine
            dustToDust 0 (read limit) matrix 0
        Nothing -> do
            putStrLn "Por favor, informar uma grade no formato suportado."
            main

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

-- Função principal para iterações
dustToDust :: Int -> Int ->  [[String]] -> Int -> IO ()
dustToDust tAtual tMax mundo tEstavel
  | tAtual < tMax = do
        let novoMundo = seeAll mundo [[""]] 0 0
        putStrLn $ "Iteração " ++ show (tAtual + 1) ++ ":"
        printMatrix novoMundo
        if novoMundo == mundo
            then do
                putStrLn $ "O sistema se estabilizou na iteração " ++ show (tAtual + 1) ++ "."
                putStrLn $ "Quantidade total de iterações: " ++ show (tAtual + 1 + tEstavel)
            else
                dustToDust (tAtual + 1) tMax novoMundo (tEstavel + 1)
  | otherwise = do
        putStrLn "A versão final do tabuleiro:"
        printMatrix mundo
        putStrLn $ "A quantidade de interações necessárias para o tabuleiro se estabilizar foi " ++ show (tEstavel + 1)

-- Função para obter o valor de uma célula em determinadas coordenadas
getValue :: [[String]] -> Int -> Int -> String
getValue mundo x y =
    if x >= 0 && y >= 0 && x < length mundo && y < length (head mundo)
        then mundo !! x !! y
        else ""

-- Função para contar o número de células vizinhas com um valor específico
countNeighbors :: [[String]] -> Int -> Int -> String -> Int
countNeighbors mundo x y value =
    let neighbors = filter (\(i, j) -> getValue mundo i j == value) (adjacentCells x y)
    in length neighbors

-- Função para obter as coordenadas das células adjacentes
adjacentCells :: Int -> Int -> [(Int, Int)]
adjacentCells x y =
    [(i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1], i /= x || j /= y]

-- Função para verificar as regras e obter o próximo estado de uma célula
updateCell :: [[String]] -> Int -> Int -> [[String]]
updateCell mundo x y
    | currentValue == "0" && liveNeighbors == 3 = [["V"]]
    | currentValue == "V" && zombieNeighbors > 0 = [["Z"]]
    | currentValue == "V" && (liveNeighbors < 2 || liveNeighbors > 3) = [["0"]]
    | currentValue == "M" && liveNeighbors == 3 = [["V"]]
    | currentValue == "Z" && liveNeighbors == 0 = [["0"]]
    | otherwise = [[currentValue]]
  where
    currentValue = getValue mundo x y
    liveNeighbors = countNeighbors mundo x y "V"
    zombieNeighbors = countNeighbors mundo x y "Z"

-- Função para criar a próxima iteração do tabuleiro
seeAll :: [[String]] -> [[String]] -> Int -> Int ->  [[String]]
seeAll mundo futuro x y
  | length mundo == length futuro && length (head mundo) == length (head futuro) =  futuro
  | otherwise = if x < length mundo
                then seeAll mundo (futuro <> updateCell mundo x y) (x + 1) y
                else  seeAll mundo (futuro <> updateCell mundo x y) 0 (y + 1)
