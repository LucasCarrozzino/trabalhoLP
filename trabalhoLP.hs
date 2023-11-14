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

-- Função para simular vida
dustToDust :: Int -> Int -> [[String]] -> Int -> Maybe [[String]]
dustToDust tAtual tMax mundo tEstavel
  | tAtual <= tMax = dustToDust (tAtual+1) tMax mundo tAtual -- Deve alterar mundo e ver se tEstavel vira tAtual ou permanece o mesmo
  | tAtual > tMax = return mundo --Ele deve dar um print em tEstavel
  | otherwise = Nothing

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
            case dustToDust 0 (read limit) matrix 0 of
                Just tabuleiro -> do
                    printMatrix tabuleiro
                Nothing -> do
                    putStrLn "Por favor, informar um número natural"
                    main
        Nothing -> do
            putStrLn "Por favor, informar um tabuleiro no formato suportado"
            main
