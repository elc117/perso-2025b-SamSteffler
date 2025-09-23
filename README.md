[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7NMOLXjY)

# Atividade avaliativa - Paradigmas de Programação

**Nome:** Samuel Steffler \
**Curso:** Ciência da Computação

## Projeto escolhido: Connect 4 usando Scotty em Haskell
**Objetivo:** criar um jogo do estilo Connect 4 que possa ser jogado via Scotty, com requisições para criar novos jogos, obter estado atual do tabuleiro e realizar movimentos. O jogo deve possuir todas as características do jogo original, com detecção de vitória, jogada inválida ou empate.
\
## Desenvolvimento do projeto
**(11/09/2025):** a primeira etapa da criação do jogo foi portar os cabeçalhos utilizados no exemplo ``05-scotty-sqlite`` disponível no GitHub da disciplina de Paradigmas. Isto incluiu a definição da estrutura de dados a ser utilizada para armazenar o jogo, alguns métodos padrões para requisições (maioria copiados, pois foram alterados posteriormente) e algumas funções iniciais para a criação do jogo:
- Inicialização da estrutura de dados (``novoJogo``)
- Detecção de coluna cheia (``checkColuna``)
- Detecção da linha em que a peça será inserida (``achaLinha``)
- Inserção da peça na matriz (``inserePeca``)

---
**(12/09/2025):** a etapa a ser realizada foi a criação da lógica de detecção de vitória. O maior empecilho foi decidir como seria feita a detecção, visto que não há laços iterativos sobre matrizes utilizando diretamente índices igual temos em linguagens imperativas. Para isso, decidi criar duas listas para cada tipo de verificação (horizontal, vertical, diagonal principal e secundária), por meio de compreensão de listas. A partir dessas listas, foi utilizada a função ``consecutivo`` para realizar a contagem de peças consecutivas iguais à peça do jogador.\
A função ``realizaJogada`` foi a responsável por lidar com a lógica de checagem de validade de jogada e detecção de vitória ou empate. Foi necessário utilizar o retorno do tipo ``Either String Game`` para permitir o retorno de mensagens de erro no caso de jogadas inválidas ou jogo completo, ou o jogo atualizado de acordo com a jogada.

---
**(13/09/2025):** a última etapa foi a mais compicada, que foi a adaptação do projeto ``05-scotty-sqlite`` no contexto do jogo Connect 4. Foi decidido manter a mesma ideia de gravar o jogo na base de dados em SQLite, porém a base seria apagada toda vez que um jogo fosse reiniciado ou o servidor iniciasse. Para entender um pouco mais como estruturas como ``Either String Game`` e ``Maybe`` funcionavam com ``case of``, utilizei o Google Gemini e [ZVON](http://www.zvon.org/comp/r/ref-Haskell.html) para tirar minhas dúvidas sobre esses tipos. Outros problemas que foram resolvidos incluíram a checagem de vitória (que além de verificar na coluna ``col`` (literal) em vez de ``col - 1``), pois acessava colunas fora do escopo e as mensagens de retorno e erro.\
Foi planejada a criação de uma página web local como front-end para jogar Connect 4, mas apesar de enviar a requisição ao servidor corretamente, por conta da política CORS o navegador bloqueava a mensagem de reposta. Por isso, foi decidido testar o jogo utilizando a extensão [REST Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client) para se comunicar com o servidor. 

---
**(23/09/2025):** adicionada lógica de teste das funções principais para o funcionamento do jogo. Ela verifica se o jogo é inicializado corretamente, se a coluna é válida, qual a linha em que uma nova peça pode ser inserida,alguns casos de vitória do jogador, e simulação de jogadas. Alguns itens do projeto principal foram alterados para permitir o uso de ```assertEqual``` de forma correta. Os testes podem ser rodados utilizando o seguinte comando:
```
runhaskell projetoTester.hs
```