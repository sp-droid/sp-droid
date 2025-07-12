document.addEventListener('DOMContentLoaded', () => {
    // OVERALL PAGE
    const stageButtons = {
        stage1: document.getElementById('stage-1-button'),
        stage2: document.getElementById('stage-2-button'),
        stage3: document.getElementById('stage-3-button')
    };

    const stages = {
        stage1: document.getElementById('stage-1'),
        stage2: document.getElementById('stage-2'),
        stage3: document.getElementById('stage-3')
    };

    function showStage(stageToShow) {
        Object.values(stages).forEach(stage => stage.style.display = 'none');
        stages[stageToShow].style.display = 'block';
    }

    stageButtons.stage1.addEventListener('click', () => showStage('stage1'));
    stageButtons.stage2.addEventListener('click', () => showStage('stage2'));
    stageButtons.stage3.addEventListener('click', () => showStage('stage3'));

    // STAGE 1
    // Listen for changes in the setup form
    const setupForm = document.getElementById('setup-form');
    let N_PLAYERS_PER_TEAM;
    let N_ROUNDS;
    let participants = [];
    setupForm.addEventListener('change', setupTournament);

    // Setup tournament
    let Nplayers, NteamsPerRound, NplayersPerRound, NgamesPerRound, Ngames, gameDraft, gameScores;
    // Draft analysis
    const analysisTotal = document.getElementById('analysis-total');
    const analysisLeftOut = document.getElementById('analysis-leftout');
    const analysisGamesPerRound = document.getElementById('analysis-gamesPerRound');
    const analysisGamesTotal = document.getElementById('analysis-gamesTotal');
    const analysisProbSameTeam = document.getElementById('analysis-probSameTeam');
    const analysisProbSameGame = document.getElementById('analysis-probSameGame');

    // temp
    setupForm.elements["players-per-team"].value = 2;
    setupForm.elements["rounds"].value = 4;
    setupForm.elements["participants"].value = Array.from({ length: 48 }, (_, i) => `Player${i + 1}`).join('\n');

    // STAGE 2
    const gamesTableBody = document.querySelector('#games-table tbody');

    // STAGE 3
    const winnersTableBody = document.querySelector('#results-table tbody');

    // INITIALIZE
    showStage('stage1');
    setupTournament();
    populateGamesTable();
    populateWinnersTable();

    // STAGE 1
    function setupTournament() {
        N_PLAYERS_PER_TEAM = parseInt(setupForm.elements['players-per-team'].value);
        N_ROUNDS = parseInt(setupForm.elements['rounds'].value);
        participants = setupForm.elements['participants'].value.split('\n').map(name => ({ Name: name.trim(), scores: [], finalScore: 0 }));
        if (participants.length <= 1) { return; }
        
        Nplayers = participants.length;
        NgamesPerRound = Math.floor(Nplayers/N_PLAYERS_PER_TEAM/2);
        NteamsPerRound = NgamesPerRound * 2;
        NplayersPerRound = NteamsPerRound * N_PLAYERS_PER_TEAM;
        Ngames = NgamesPerRound * N_ROUNDS;

        analysisTotal.textContent = Nplayers;
        analysisLeftOut.textContent = Nplayers-NplayersPerRound;
        analysisGamesPerRound.textContent = NgamesPerRound;
        analysisGamesTotal.textContent = Ngames;
        analysisProbSameTeam.textContent = (100-100*Math.pow(1-(N_PLAYERS_PER_TEAM-1)/(Nplayers-1), N_ROUNDS)).toFixed(2);
        analysisProbSameGame.textContent = (100-100*Math.pow(1-(N_PLAYERS_PER_TEAM*2-1)/(Nplayers-1), N_ROUNDS)).toFixed(2);

        gameDraft = Array.from({ length: N_ROUNDS }, (_, i) => generateRoundRandom(i));
        gameScores = Array.from({ length: N_ROUNDS }, () => Array(NteamsPerRound).fill(null));
        // gameScores = Array.from({ length: N_ROUNDS }, () => generateUniqueRandomIntegers(NteamsPerRound, 44));
    }

    function generateRoundRandom(round) {
        return generateUniqueRandomIntegers(NteamsPerRound * N_PLAYERS_PER_TEAM, Nplayers - 1);
    }

    console.log(gameDraft[0])
    console.log(gameScores[0]);
    // STAGE 2
    function populateGamesTable() {
        round = 0;
        gamesTableBody.innerHTML = ''; // Clear previous rows

        for (let round = 0; round < N_ROUNDS; round++) {
            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const completeGameId = round * NgamesPerRound + gameId;
                const team1Players = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
                const team2Players = gameDraft[round].slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);
                const team1Score = gameScores[round][gameId * 2];
                const team2Score = gameScores[round][gameId * 2 + 1];

                let score;
                if (team1Score === null) {
                    score = `<span class="introduceResult" style="color: rgb(0, 0, 238);"  onclick="window.introduceResult(${round}, ${gameId})">Pending</span>`;
                } else {
                    score = `<span class="introduceResult" style="color: rgb(0, 120, 150);" onclick="window.introduceResult(${round}, ${gameId})">${team1Score} - ${team2Score}</span>`;
                }

                const row = document.createElement('tr');

                row.innerHTML = `
                    <td>${completeGameId + 1}</td>
                    <td>${round+1   }</td>
                    <td>${team1Players.map(playerId => participants[playerId].Name).join(', ')}</td>
                    <td>${team2Players.map(playerId => participants[playerId].Name).join(', ')}</td>
                    <td>${score}</td>
                `;

                gamesTableBody.appendChild(row);
            }
        }
    }

    window.introduceResult = function(round, gameId) {
        const overlay = document.createElement('div');
        overlay.style.position = 'fixed';
        overlay.style.top = '0';
        overlay.style.left = '0';
        overlay.style.width = '100%';
        overlay.style.height = '100%';
        overlay.style.backgroundColor = 'rgba(0, 0, 0, 0.5)';
        overlay.style.zIndex = '999';
        document.body.appendChild(overlay);
        
        const popup = document.createElement('div');
        popup.style.position = 'fixed';
        popup.style.top = '50%';
        popup.style.left = '50%';
        popup.style.transform = 'translate(-50%, -50%)';
        popup.style.backgroundColor = '#fff';
        popup.style.padding = '20px';
        popup.style.boxShadow = '0 0 10px rgba(0, 0, 0, 0.3)';
        popup.style.zIndex = '1000';

        popup.innerHTML = `
            <h3>Enter Scores for Game ${gameId}:</h3>
            <label>Team 1 score:</label>
            <input type="number" id="team1-score" placeholder="Enter score" />
            <br><br>
            <label>Team 2 score:</label>
            <input type="number" id="team2-score" placeholder="Enter score" />
            <br><br>
            <div style="display: flex; justify-content: center; gap: 10px;">
                <button id="ok-button" style="background-color: #4CAF50; color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer;">Add result</button>
                <button id="cancel-button" style="background-color: #f44336; color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer;">Cancel</button>
            </div>
        `;

        document.body.appendChild(popup);

        document.getElementById('ok-button').addEventListener('click', () => {
            const team1Score = document.getElementById('team1-score').value;
            const team2Score = document.getElementById('team2-score').value;

            if (team1Score === '' || team2Score === '') {
                alert('Please enter scores for both teams.');
                return;
            }
            document.body.removeChild(popup); document.body.removeChild(overlay);

            gameScores[round][gameId * 2] = parseInt(team1Score);
            gameScores[round][gameId * 2 + 1] = parseInt(team2Score);
            populateGamesTable(); // Refresh the games table to show the new scores
            assignScores(); // Assign scores to participants based on the updated game scores
            populateWinnersTable(); // Refresh the winners table to show the new scores
        });
        document.getElementById('cancel-button').addEventListener('click', () => {
            document.body.removeChild(popup); document.body.removeChild(overlay);
        });
    }
    function assignScores() {
        for (const participant of participants) {
            participant.scores = []; // Reset scores for each participant
        }
        for (let round = 0; round < N_ROUNDS; round++) {
            for (let i = 0; i < NteamsPerRound*N_PLAYERS_PER_TEAM; i++) {
                const playerId = gameDraft[round][i]
                const score = gameScores[round][Math.floor(i / N_PLAYERS_PER_TEAM)];
                if (score !== null) { participants[playerId]["scores"].push(score); }
            }
        }
    }

    // STAGE 3
    function populateWinnersTable() {
        winnersTableBody.innerHTML = ''; // Clear previous rows

        const finalScores = participants.map((participant, index) => {
            let totalScore;
            if (participant.scores.length === 0) {
                totalScore = 0;
            } else {
                totalScore = participant.scores.reduce((sum, score) => sum + score, 0) / participant.scores.length; // Calculate average score
            }
            return { Name: participant.Name, Results: participant.scores, Score: totalScore };
        });

        finalScores.sort((a, b) => b.Score - a.Score); // Sort by score descending

        finalScores.forEach((participant, rank) => {
            const row = document.createElement('tr');
            row.innerHTML = `
                <td>${rank + 1}</td>
                <td>${participant.Name}</td>
                <td>${participant.Results.join(', ')}</td>
                <td>${participant.Score}</td>
            `;
            winnersTableBody.appendChild(row);
        });
    }
});

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