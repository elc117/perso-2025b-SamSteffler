<template>
  <div>
    <h1>Connect 4</h1>
    <div class="controls">
      <button @click="resetGame">Reiniciar Jogo</button>
      <button @click="fetchGameState">Atualizar Jogo</button>
    </div>
    <div class="move-input">
      <input type="number" v-model.number="column" placeholder="Coluna (1-7)">
      <button @click="placePiece">Colocar Peça</button>
    </div>
    <div v-if="game" class="game-status">
      <p>Jogador Atual: {{ game.currentPlayer }}</p>
      <p v-if="game.isOver">Fim de Jogo!</p>
      <p v-if="game.winner">Vencedor: {{ game.winner }}</p>
    </div>
    <div v-if="game" class="board">
      <div v-for="(row, rowIndex) in game.matrix" :key="rowIndex" class="row">
        <div v-for="(cell, colIndex) in row" :key="colIndex" class="cell">
          <span v-if="cell === 1" class="piece player1">O</span>
          <span v-else-if="cell === 2" class="piece player2">O</span>
          <span v-else>&nbsp;</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue';

const game = ref(null);
const column = ref(null);

const fetchGameState = async () => {
  try {
    const response = await fetch('http://localhost:3000/jogo');
    if (response.ok) {
      game.value = await response.json();
    } else {
      console.error('Failed to fetch game state');
    }
  } catch (error) {
    console.error('Error fetching game state:', error);
  }
};

const resetGame = async () => {
  try {
    const response = await fetch('http://localhost:3000/criajogo', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
    });
    if (response.ok) {
      await fetchGameState();
    } else {
      console.error('Failed to reset game');
    }
  } catch (error) {
    console.error('Error resetting game:', error);
  }
};

const placePiece = async () => {
  if (column.value === null || column.value < 1 || column.value > 7) {
    alert('Please enter a valid column number (1-7).');
    return;
  }
  try {
    const response = await fetch('http://localhost:3000/jogo/move', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ jogoColuna: column.value }),
    });
    if (response.ok) {
      await fetchGameState();
    } else {
      const errorData = await response.json();
      alert(`Error: ${errorData}`);
    }
  } catch (error) {
    console.error('Error placing piece:', error);
  }
  column.value = null;
};

onMounted(() => {
  fetchGameState();
});
</script>

<style>
.board {
  display: inline-block;
  border: 2px solid #333;
  background-color: #4a90e2;
  padding: 10px;
  border-radius: 10px;
}

.row {
  display: flex;
}

.cell {
  width: 50px;
  height: 50px;
  border: 1px solid #333;
  display: flex;
  justify-content: center;
  align-items: center;
  background-color: white;
  margin: 2px;
  border-radius: 50%;
}

.piece {
  font-size: 36px;
  line-height: 50px;
  border-radius: 50%;
  width: 45px;
  height: 45px;
  text-align: center;
}

.player1 {
  color: #d0021b;
}

.player2 {
  color: #f5a623;
}

.controls, .move-input, .game-status {
  margin-bottom: 15px;
}

button {
  margin-left: 10px;
}
</style>