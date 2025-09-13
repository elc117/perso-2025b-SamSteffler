{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status400, status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Text.Read (readMaybe, look)

-- corpo para visualizacao do jogo
data Game = Game
    { gameId :: Int             -- id do jogo (permite varios jogos na database)
    , isOver :: Bool            -- se o jogo acabou
    , line :: Int               -- total de linhas
    , column :: Int             -- total de colunas
    , matrix :: [[Int]]         -- matriz do jogo (0 = vazio, 1 = jogador 1, 2 = jogador 2)
    , currentPlayer :: Int      -- jogador atual (1 ou 2)
    , winner :: Maybe Int       -- vencedor (Null se ninguem ganhou ainda, 1 ou 2 se alguem ganhou)
    } deriving (Show, Generic)


instance ToJSON Game
instance FromJSON Game

-- corpo para fazer movimento no jogo
data MoveRequest = MoveRequest
    { jogoColuna :: Int             -- coluna para jogar
    } deriving (Show, Generic)

instance FromJSON MoveRequest

-- interpreta a matriz do jogo de [[Int]] para TEXT para escrita na base de dados
instance ToField [[Int]] where
    toField :: [[Int]] -> SQLData
    toField = toField . T.pack . show

-- interpreta a matriz do jogo de TEXT para [[Int]] para leitura na base de dados
instance FromField [[Int]] where
    fromField f = do
        s <- fromField f
        case readMaybe (T.unpack s) of
            Just val -> return val
            -- retorna erro com o campo original
            Nothing  -> returnError ConversionFailed f "Matriz nao pode ser convertida"

-- interpreta a linha da database para o corpo do jogo
instance FromRow Game where
    fromRow :: RowParser Game
    fromRow = Game <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- inicializa a base de dados dos jogos
initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS game (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ isOver BOOLEAN,\
  \ line INTEGER,\
  \ column INTEGER,\
  \ matrix TEXT,\
  \ currentPlayer INTEGER,\
  \ winner INTEGER)"

-- metodo para criar um novo jogo
-- passar linhas e colunas
novoJogo :: Int -> Int -> Game
novoJogo l c = Game
    { gameId = -1 -- id do jogo sera decidido em breve
    , isOver = False
    , line = l
    , column = c
    , matrix = replicate l (replicate c 0)  -- matriz inicial vazia
    , currentPlayer = 1                     -- jogador 1 sempre comeca
    , winner = Nothing
    }

-- verificacao de coluna cheia, retorna true se estiver cheia
checkColuna :: Int -> Game -> Bool
-- pega a primeira linha e compara com o item na posicao col
checkColuna col jogo = (head (matrix jogo) !! col) /= 0

achaLinha :: Int -> [[Int]] -> Int
achaLinha c mat = busca (length mat - 1) -- comeca da ultima linha (indice length - 1)
  where
    busca (-1) = -1  -- coluna cheia (redundancia para em casos de erro)
    busca r
      | (mat !! r !! c) == 0 = r -- checa se posicao c da linha r esta vazia
      | otherwise            = busca (r - 1) -- busca "recursiva" se nao achar

inserePeca :: Int -> Int -> [[Int]] -> [[Int]]
inserePeca c p mat =
    let l = achaLinha c mat
    in if l == -1
        -- posicao invalida pois coluna esta cheia, retornar matriz original (redundancia)
        then mat
        -- fazer movimento: copia as linhas antes de l, insere peca
        -- na linha l posicao c e copia o restante das linhas depois de l
        else take l mat ++ [take c (mat !! l) ++ [p] ++ drop (c + 1) (mat !! l)] ++ drop (l + 1) mat

-- Either String Game permite retornar erro via String ou o próprio Game atualizado
realizarJogada :: Int -> Game -> Either String Game
realizarJogada col jogo
    | isOver jogo                    = Left $ "Jogo já está terminado. Vencedor: " ++ show (winner jogo)
    | col <= 0 || col > column jogo  = Left "Coluna inválida."
    | checkColuna (col - 1) jogo     = Left "Coluna cheia."
    | otherwise                      = Right atualizaJogada
    where
        -- linha onde sera inserida a peca
        linhaVazia = achaLinha (col - 1) (matrix jogo)
        -- guarda matriz atualizada
        matAtualizada = inserePeca (col - 1) (currentPlayer jogo) (matrix jogo)
        -- guarda proximo jogador
        proximoJogador = if currentPlayer jogo == 1 then 2 else 1
        -- atualiza jogo atual
        jogoFeito = jogo { matrix = matAtualizada }
        -- checam vitoria e empate
        isVitoria = vitoria jogoFeito linhaVazia (col - 1)
        isEmpate = empate matAtualizada
        
        -- verifica se houve vitoria ou empate e atualiza o jogo
        atualizaJogada = case isVitoria of
            Just ganhador  -> jogoFeito { isOver = True, winner = Just ganhador }
            Nothing -> if isEmpate
                then jogoFeito { isOver = True, winner = Nothing }
                else jogoFeito { currentPlayer = proximoJogador }



-- takewhile pega so elementos enquanto forem iguais a p depois para
consecutivo :: [Int] -> Int -> [Int]
consecutivo arr p = takeWhile (== p) arr

-- checagem de vitoria horizontal
vitoriaHorizontal :: [[Int]] -> Int -> Int -> Int -> Bool
vitoriaHorizontal mat l c p = (1 + length horesq + length hordir) >= 4
    where
        horesq = consecutivo (reverse (take c (mat !! l))) p
        hordir = consecutivo (drop (c + 1) (mat !! l)) p

-- checagem de vitoria vertical
vitoriaVertical :: [[Int]] -> Int -> Int -> Int -> Bool
vitoriaVertical mat l c p = (1 + length vertcima + length vertbaixo) >= 4
    where
        coluna  = [row !! c | row <- mat]
        vertcima = consecutivo (reverse (map (!! c) (take l mat))) p
        vertbaixo = consecutivo (map (!! c) (drop (l + 1) mat)) p

-- checagem de vitoria diagonal
vitoriaDiag :: [[Int]] -> Int -> Int -> Int -> Bool
vitoriaDiag mat l c p = (1 + length diag1cima + length diag1baixo >= 4) ||
                        (1 + length diag2cima + length diag2baixo >= 4)
    where
        -- diagonal principal para cima (indices diminuem)
        diag1cima = consecutivo [mat !! (l - i) !! (c - i) | i <- [1..min l c], (l - i) >= 0, (c - i) >= 0] p
        -- diagonal principal para baixo (indices aumentam)
        diag1baixo = consecutivo [mat !! (l + i) !! (c + i) | i <- [1..min (length mat - l - 1) (length (head mat) - c - 1)], (l + i) < length mat, (c + i) < length (head mat)] p
        -- diagonal secundaria para cima (vertical diminui, horizontal aumenta)
        diag2cima = consecutivo [mat !! (l - i) !! (c + i) | i <- [1..min l (length (head mat) - c - 1)], (l - i) >= 0, (c + i) < length (head mat)] p
        -- diagonal secundaria para baixo (vertical aumenta, horizontal diminui)
        diag2baixo = consecutivo [mat !! (l + i) !! (c - i) | i <- [1..min (length mat - l - 1) c], (l + i) < length mat, (c - i) >= 0] p

-- checagem de empate
empate :: [[Int]] -> Bool
-- concatena matriz bidimensional em unidimencional e aplica funcao notElem
empate mat = 0 `notElem` concat mat

-- retorna jogador se tiver vitoria senao retorna Nothing
vitoria :: Game -> Int -> Int -> Maybe Int
vitoria jogo l c =
    let jogador = currentPlayer jogo
    in if vitoriaHorizontal (matrix jogo) l c jogador ||
          vitoriaVertical (matrix jogo) l c jogador ||
          vitoriaDiag (matrix jogo) l c jogador
       then Just jogador
       else Nothing


-- corpo principal do programa
main :: IO ()
main = do
    conn <- open "games.db"
    initDB conn
    -- apaga database sempre que iniciar o servidor
    liftIO $ execute_ conn "DELETE FROM game"
    -- inicializar servidor
    port <- maybe 3000 read <$> lookupEnv "PORT"
    putStrLn $ "Starting server on port " ++ show port
    putStrLn $ "Local website : http://localhost:" ++ show port
    -- rotas do Scotty
    scotty port $ do
        middleware logStdoutDev

        get "/" $ do
            text $ T.pack ("Bem-vindo ao jogo de Connect 4! Use http://localhost:" ++ show port ++ "/criajogo para iniciar um novo jogo.")
        -- criar novo jogo
        post "/criajogo" $ do
            let jogo = novoJogo 6 7
            liftIO $ execute_ conn "DELETE FROM game"
            liftIO $ execute conn "INSERT INTO game (isOver, line, column, matrix, currentPlayer, winner) VALUES (?, ?, ?, ?, ?, ?)"
                        (isOver jogo, line jogo, column jogo, matrix jogo, currentPlayer jogo, winner jogo)
            json (T.pack ("Jogo criado com sucesso! Você pode jogar pelo link http://localhost:" ++ show port ++ "/jogo/move/ e a coluna desejada."))

        -- fazer jogada
        post "/jogo/move" $ do
            colParam <- jsonData :: ActionM MoveRequest
            let colunaParam = jogoColuna colParam
            -- pegar o jogo atual (deve haver apenas um jogo na database)
            jogos <- liftIO $ query_ conn "SELECT id, isOver, line, column, matrix, currentPlayer, winner FROM game" :: ActionM [Game]
            if null jogos
                then status status404 >> json ("Nenhum jogo encontrado. Crie um novo jogo primeiro." :: String)
                else do
                    let jogoAtual = head jogos
                    case realizarJogada colunaParam jogoAtual of
                        Left errMsg -> status status400 >> json (errMsg :: String)
                        Right jogoAtualizado -> do
                            -- atualizar o jogo na database
                            liftIO $ execute conn "UPDATE game SET isOver = ?, matrix = ?, currentPlayer = ?, winner = ? WHERE id = ?"
                                (isOver jogoAtualizado, matrix jogoAtualizado, currentPlayer jogoAtualizado, winner jogoAtualizado, gameId jogoAtualizado)
                            json jogoAtualizado

        -- pegar estado do jogo
        get "/jogo" $ do
            jogos <- liftIO $ query_ conn "SELECT id, isOver, line, column, matrix, currentPlayer, winner FROM game" :: ActionM [Game]
            if null jogos
                then status status404 >> json ("Nenhum jogo encontrado. Crie um novo jogo primeiro." :: String)
                else json (head jogos)