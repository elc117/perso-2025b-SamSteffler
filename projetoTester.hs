module Main where
import Projeto hiding (main)
import Test.HUnit

-- matrizes de teste
testmat = [[0,1,0,0,0,0,0],
           [0,2,2,0,0,0,0],
           [2,1,2,0,0,0,0],
           [1,1,1,0,0,1,0],
           [2,2,2,0,2,2,0],
           [1,1,1,2,1,1,0]]
testmat1 = [[0,2,0,0,0,0,0],
            [0,1,2,0,0,0,0],
            [2,1,1,0,0,0,0],
            [1,2,1,2,0,0,0],
            [2,1,2,1,0,0,0],
            [1,2,1,2,1,0,0]]
testmat2 = [[0,2,0,0,0,0,0],
            [0,1,2,0,0,0,0],
            [2,1,1,0,0,0,0],
            [1,2,1,2,0,0,0],
            [2,1,2,1,0,0,0],
            [1,2,1,2,1,0,0]]
testmat3 = [[0,0,0,2,0,0,0],
            [0,0,0,2,0,0,0],
            [0,0,1,2,0,0,0],
            [0,1,1,2,0,0,0],
            [2,2,2,1,0,0,0],
            [1,1,1,1,0,0,0]]
testmat4 = [[1,2,1,2,1,2,0],
            [2,1,2,1,2,1,2],
            [1,2,1,2,2,2,1],
            [2,1,2,1,2,1,2],
            [1,2,1,2,1,2,1],
            [2,1,2,1,2,1,2]]

-- teste para criação de jogos em branco
testNovoJogo :: Test
testNovoJogo = TestCase $ do
    assertEqual "Teste de matriz vazia" mat (matrix game)
    assertEqual "Teste de jogador inicial" 1 (currentPlayer game)
    assertEqual "Teste de ganhador inicial" Nothing (winner game)
    assertEqual "Teste de jogo" False (isOver game)
    where 
        game = novoJogo 6 7
        mat = replicate 6 (replicate 7 0)

-- teste para coluna cheia
testColunaCheia :: Test
testColunaCheia = TestCase $ do
    assertEqual "Teste de coluna cheia" (checkColuna 1 jogo) True
    assertEqual "Teste de coluna nao cheia" (checkColuna 0 jogo) False
        where
            jogo = (novoJogo 6 7) { matrix = testmat }

-- teste para obter linha para inserir peça
testLinha :: Test
testLinha = TestCase $ do
    assertEqual "Teste retorno de linha coluna 1" (achaLinha 0 testmat) 1
    assertEqual "Teste retorno de linha coluna 2" (achaLinha 1 testmat) (-1)
    assertEqual "Teste retorno de linha coluna 3" (achaLinha 2 testmat) 0
    assertEqual "Teste retorno de linha coluna 4" (achaLinha 3 testmat) 4
    assertEqual "Teste retorno de linha coluna 5" (achaLinha 4 testmat) 3
    assertEqual "Teste retorno de linha coluna 6" (achaLinha 5 testmat) 2
    assertEqual "Teste retorno de linha coluna 7" (achaLinha 6 testmat) 5

-- teste função consecutivo
testConsecutivo :: Test
testConsecutivo = TestCase $ do
    assertEqual "Teste consecutivo 1" (consecutivo (replicate 4 1 ++ replicate 3 0) 1) (replicate 4 1)
    assertEqual "Teste consecutivo 1" (consecutivo (replicate 4 2 ++ replicate 3 0) 1) []

-- teste função vitoria em diferentes jogos
testVitorias :: Test
testVitorias = TestCase $ do
    assertEqual "Teste vitórias jogo 1" (vitoria jogo1 3 5) Nothing
    assertEqual "Teste vitórias jogo 2" (vitoria jogo2 3 3) Nothing
    assertEqual "Teste vitórias jogo 3" (vitoria jogo3 2 0) Nothing
    assertEqual "Teste vitórias jogo 4" (vitoria jogo4 0 3) (Just 2)
    where
        jogo1 = (novoJogo 6 7) { matrix = testmat, currentPlayer = 1 }
        jogo2 = (novoJogo 6 7) { matrix = testmat1, currentPlayer = 2 }
        jogo3 = (novoJogo 6 7) { matrix = testmat2, currentPlayer = 1 }
        jogo4 = (novoJogo 6 7) { matrix = testmat3, currentPlayer = 2 }

-- teste para diferentes casos de jogadas
testJogada :: Test
testJogada = TestCase $ do
    assertEqual "Teste jogada válida" (realizarJogada 7 jogo) (Right jogoEsperado)
    assertEqual "Teste jogada inválida" (realizarJogada 2 jogo) (Left "Coluna cheia.")
    assertEqual "Teste vitória" (realizarJogada 4 jogo) (Right jogoEsperado2)
    assertEqual "Teste empate" (realizarJogada 7 jogoPreEmpate) (Right jogoEmpate)
    assertEqual "Teste jogada inválida (fora do intervalo)" (realizarJogada 8 jogo) (Left "Coluna inválida.")
    assertEqual "Teste jogada após vitória" (realizarJogada 7 jogoEsperado2) (Left "Jogo já está terminado. Vencedor: Just 1")
    assertEqual "Teste jogada após empate" (realizarJogada 7 jogoEmpate) (Left "Jogo já está terminado. Vencedor: Nothing")
    where
        jogo = (novoJogo 6 7) { matrix = testmat, currentPlayer = 1 }
        jogoEsperado = jogo { matrix = novaMat, currentPlayer = 2 }
        novaMat = [[0,1,0,0,0,0,0],
                   [0,2,2,0,0,0,0],
                   [2,1,2,0,0,0,0],
                   [1,1,1,0,0,1,0],
                   [2,2,2,0,2,2,0],
                   [1,1,1,2,1,1,1]]
        jogoEsperado2 = jogoEsperado { matrix = winmat, winner = Just 1, currentPlayer = 1, isOver = True }
        winmat = [[0,1,0,0,0,0,0],
                  [0,2,2,0,0,0,0],
                  [2,1,2,0,0,0,0],
                  [1,1,1,0,0,1,0],
                  [2,2,2,1,2,2,0],
                  [1,1,1,2,1,1,0]]
        jogoPreEmpate = (novoJogo 6 7) { matrix = testmat4, currentPlayer = 1, isOver = False }
        jogoEmpate = (novoJogo 6 7) { matrix = empatemat, currentPlayer = 1, isOver = True, winner = Nothing }
        empatemat = [[1,2,1,2,1,2,1],
                    [2,1,2,1,2,1,2],
                    [1,2,1,2,2,2,1],
                    [2,1,2,1,2,1,2],
                    [1,2,1,2,1,2,1],
                    [2,1,2,1,2,1,2]]

tests :: Test
tests = TestList [testNovoJogo, testColunaCheia, testLinha, testConsecutivo, testVitorias, testJogada]

main :: IO()
main = do
    putStrLn "Rodando testes..."
    runTestTT tests
    return ()