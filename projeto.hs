{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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

-- corpo para fazer movimento no jogo
data MoveRequest = MoveRequest
    { column :: Int             -- coluna para jogar
    } deriving (Show, Generic)

instance FromJSON Game
instance ToJSON Game

instance FromJSON MoveRequest

-- interpreta a linha da database para o corpo do jogo
instance FromRow Game where
    fromRow = Game <$> field <*> field <*> field <*> field <*> field <*> field

-- interpreta a matriz do jogo de [[Int]] para TEXT para escrita na base de dados
instance ToField [[Int]] where
    toField = toField . T.pack . show

-- interpreta a matriz do jogo de TEXT para [[Int]] para leitura na base de dados
instance FromField [[Int]] where
    fromField f = do
        s <- fromField f
        case readMaybe (T.unpack s) of
            Just val -> return val
            -- retorna erro com o campo original
            Nothing  -> returnError ConversionFailed f "Matriz nao pode ser convertida"

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
    | isOver jogo                    = Left "Jogo já está terminado."
    | col <= 0 || col >= column jogo = Left "Coluna inválida."
    | checkColuna (col - 1) jogo     = Left "Coluna cheia."
    | otherwise                      = Right atualizaJogada
    where 
        matAtualizada = inserePeca (col - 1) (currentPlayer jogo) (matrix jogo)
        proximoJogador = if currentPlayer jogo == 1 then 2 else 1
        -- criar logica de vitoria

main :: IO ()
main = do
    conn <- open "games.db"
    initDB conn