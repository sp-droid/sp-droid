N_PLAYERS_PER_TEAM = 2;
N_ROUNDS = 4;

participants = Array.from({ length: 17 }, (_, i) => ({ Name: i+1, scores: [], finalScore: 0 }));
Nplayers = participants.length;

NteamsPerRound = Math.floor(Nplayers/N_PLAYERS_PER_TEAM);
NgamesPerRound = NteamsPerRound / 2;
Ngames = NgamesPerRound * N_ROUNDS;

game = []
gameScores = []

for (let round = 0; round < N_ROUNDS; round++) {
    // Organize teams for the round
    generateRoundRandom(round);
    // Simulate game
    gameScores[0] = generateUniqueRandomIntegers(NteamsPerRound, 21);
    // Assign scores to players
    assignScores(round);
}

for (let partipant of participants) {
    partipant.finalScore = parseFloat((partipant.scores.reduce((acc, score) => acc + score, 0) / partipant.scores.length).toFixed(2));
}
participants = participants.slice().sort((a, b) => b.finalScore - a.finalScore);
console.log(participants);

function generateRoundRandom(round) {
    game[round] = generateUniqueRandomIntegers(NteamsPerRound * N_PLAYERS_PER_TEAM, Nplayers - 1);
}

function assignScores(round) {
    for (let i = 0; i < NteamsPerRound*N_PLAYERS_PER_TEAM; i++) {
        player = game[round][i]
        score = gameScores[0][Math.floor(i / N_PLAYERS_PER_TEAM)];
        participants[player]["scores"].push(score);
    }
}

function generateUniqueRandomIntegers(Ngenerated, maxValue) {
    if (Ngenerated > maxValue + 1) {
        throw new Error("Ngenerated cannot be greater than maxValue + 1.");
    }

    const result = new Set(); // Use a Set to ensure uniqueness
    while (result.size < Ngenerated) {
        const randomInt = Math.floor(Math.random() * (maxValue + 1)); // Random integer between 0 and N
        result.add(randomInt);
    }

    return Array.from(result); // Convert the Set to an array
}