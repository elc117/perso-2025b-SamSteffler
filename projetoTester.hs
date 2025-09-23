module Main where
import Projeto
import Test.HUnit

testmat = [[0,1,0,0,0,0,0],
           [0,2,2,0,0,0,0],
           [2,1,2,0,0,0,0],
           [1,1,1,0,0,1,0],
           [2,2,2,0,2,2,0],
           [1,1,1,1,1,1,0]]
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

testNovoJogo :: Test
testNovoJogo = TestCase (assertEqual "Teste de matriz vazia" mat (matrix (novoJogo 6 7)))
    where mat = replicate 6 (replicate 7 0)

testColunaCheia :: Test
testColunaCheia = TestCase $ do
    assertEqual "Teste de coluna cheia" (checkColuna 1 jogo) True
    assertEqual "Teste de coluna nao cheia" (checkColuna 0 jogo) False
        where
            jogo = (novoJogo 6 7) { matrix = testmat }

testLinha :: Test
testLinha = TestCase $ do
    assertEqual "Teste retorno de linha coluna 1" (achaLinha 0 testmat) 1
    assertEqual "Teste retorno de linha coluna 2" (achaLinha 1 testmat) (-1)
    assertEqual "Teste retorno de linha coluna 3" (achaLinha 2 testmat) 0
    assertEqual "Teste retorno de linha coluna 4" (achaLinha 3 testmat) 4
    assertEqual "Teste retorno de linha coluna 5" (achaLinha 4 testmat) 3
    assertEqual "Teste retorno de linha coluna 6" (achaLinha 5 testmat) 2
    assertEqual "Teste retorno de linha coluna 7" (achaLinha 6 testmat) 5

testConsecutivo :: Test
testConsecutivo = TestCase $ do
    assertEqual "Teste consecutivo 1" (consecutivo (replicate 4 1 ++ replicate 3 0) 1) (replicate 4 1)
    assertEqual "Teste consecutivo 1" (consecutivo (replicate 4 2 ++ replicate 3 0) 1) []

testVitorias :: Test
testVitorias = TestCase $ do
    assertEqual "Teste vitórias jogo 1" (vitoria jogo1 5 3) (Just 1)
    assertEqual "Teste vitórias jogo 2" (vitoria jogo2 3 3) Nothing
    assertEqual "Teste vitórias jogo 3" (vitoria jogo3 2 0) Nothing
    assertEqual "Teste vitórias jogo 4" (vitoria jogo4 0 3) (Just 2)
    where
        jogo1 = (novoJogo 6 7) { matrix = testmat, currentPlayer = 1 }
        jogo2 = (novoJogo 6 7) { matrix = testmat1, currentPlayer = 2 }
        jogo3 = (novoJogo 6 7) { matrix = testmat2, currentPlayer = 1 }
        jogo4 = (novoJogo 6 7) { matrix = testmat3, currentPlayer = 2 }

tests :: Test
tests = TestList [testNovoJogo, testColunaCheia, testLinha, testConsecutivo, testVitorias]

main :: IO()
main = do
    putStrLn "Rodando testes..."
    runTestTT tests
    return ()