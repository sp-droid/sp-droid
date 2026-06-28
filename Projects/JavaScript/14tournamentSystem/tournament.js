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
    stageButtons.stage2.addEventListener('click', () => {
        showStage('stage2');
        if (gameDraft) populateGamesTable();
    });
    stageButtons.stage3.addEventListener('click', () => {
        showStage('stage3');
        if (gameDraft) populateWinnersTable();
    });

    // STAGE 1
    const tournamentNameInput = document.getElementById('tournament-name');
    const pageTitle = document.querySelector('title');
    const displayedTitle = document.querySelector('header h1');

    tournamentNameInput.addEventListener('input', () => {
        const newName = tournamentNameInput.value || 'Tournament tool';
        pageTitle.textContent = newName;
        displayedTitle.textContent = newName;
    });
    
    // Listen for changes in the setup form
    const setupForm = document.getElementById('setup-form');
    let N_PLAYERS_PER_TEAM;
    let N_ROUNDS;
    let participants = [];
    setupForm.addEventListener('change', setupTournament);

    // Setup tournament
    let Nplayers, NteamsPerRound, NplayersPerRound, NgamesPerRound, Ngames, gameDraft, gameScores, gameCourts, pairingHistory;
    // Draft analysis
    const analysisTotal = document.getElementById('analysis-total');
    const analysisLeftOut = document.getElementById('analysis-leftout');
    const analysisGamesPerRound = document.getElementById('analysis-gamesPerRound');
    const analysisGamesTotal = document.getElementById('analysis-gamesTotal');
    const analysisDuplicateNames = document.getElementById('analysis-duplicateNames');

    // temp
    setupForm.elements["players-per-team"].value = 2;
    setupForm.elements["rounds"].value = 4;
    setupForm.elements["participants"].value = `Spencer, Rafael, Wayne, Trystan, Jovan, Susan, Charlie, Regan, Samaria, Yulisa, Bella, Jadah, Alyssia, Braeden, Savana, Auston, Nolan, Alfonso, Keelan, Erik, Bayley, Deonte, Janeth, Tyquan, Erich, Corrina, Trevin, Mayra, Cameryn, Maximus, Peter, Janie, Max, Kerry, Serena, Cristal, Ramon, Humberto, Corey, Ivan, Jaden, Phoenix, Montana, Karley, Destini, Tamia, Skyla, Avery`.split(', ').join('\n');

    // STAGE 2
    const gamesTableBody = document.querySelector('#games-table tbody');
    const highlightedPlayerSpan = document.getElementById('highlighted-player');
    const playsWithTeamSpan = document.getElementById('plays-with-team');
    const playsWithGameSpan = document.getElementById('plays-with-game');
    let currentHighlightedPlayer = null;
    let extraMatches = [];

    // Add Download CSV button before games table
    const gamesTable = document.getElementById('games-table');
    if (gamesTable) {
        const downloadBtn = document.createElement('button');
        downloadBtn.id = 'download-csv';
        downloadBtn.textContent = 'Download CSV';
        downloadBtn.style.backgroundColor = '#4CAF50';
        downloadBtn.style.color = 'white';
        downloadBtn.style.border = 'none';
        downloadBtn.style.padding = '10px 20px';
        downloadBtn.style.borderRadius = '5px';
        downloadBtn.style.cursor = 'pointer';
        downloadBtn.style.marginBottom = '10px';
        gamesTable.parentNode.insertBefore(downloadBtn, gamesTable);

        downloadBtn.addEventListener('click', () => {
            let csv = [];
            // Get headers
            const thead = gamesTable.querySelector('thead');
            if (thead) {
                const headers = Array.from(thead.querySelectorAll('th')).map(th => '"' + th.textContent.trim() + '"');
                csv.push(headers.join(';'));
            }
            // Get rows
            const rows = gamesTable.querySelectorAll('tbody tr');
            rows.forEach(row => {
                const cols = Array.from(row.children).map((td, idx, arr) => {
                    let text = td.textContent.trim();
                    // If this is the last column (score), prefix with a single quote to prevent Excel date conversion
                    if (idx === arr.length - 1) {
                        text = "'" + text;
                    }
                    return '"' + text + '"';
                });
                csv.push(cols.join(';'));
            });
            // Download
            const csvContent = csv.join('\r\n');
            const blob = new Blob([csvContent], { type: 'text/csv' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'games_table.csv';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
        });

        // Add "Add match" button
        const addMatchBtn = document.createElement('button');
        addMatchBtn.id = 'add-match';
        addMatchBtn.textContent = 'Add match';
        addMatchBtn.style.backgroundColor = '#ff9800';
        addMatchBtn.style.color = 'white';
        addMatchBtn.style.border = 'none';
        addMatchBtn.style.padding = '10px 20px';
        addMatchBtn.style.borderRadius = '5px';
        addMatchBtn.style.cursor = 'pointer';
        addMatchBtn.style.marginBottom = '10px';
        addMatchBtn.style.marginLeft = '10px';
        gamesTable.parentNode.insertBefore(addMatchBtn, gamesTable);

        addMatchBtn.addEventListener('click', () => {
            window.showAddMatchDialog();
        });
    }

    // STAGE 3
    const winnersTableBody = document.querySelector('#results-table tbody');

    // Add Download CSV button before results table
    const resultsTable = document.getElementById('results-table');
    if (resultsTable) {
        const downloadBtnResults = document.createElement('button');
        downloadBtnResults.id = 'download-csv-results';
        downloadBtnResults.textContent = 'Download CSV';
        downloadBtnResults.style.backgroundColor = '#4CAF50';
        downloadBtnResults.style.color = 'white';
        downloadBtnResults.style.border = 'none';
        downloadBtnResults.style.padding = '10px 20px';
        downloadBtnResults.style.borderRadius = '5px';
        downloadBtnResults.style.cursor = 'pointer';
        downloadBtnResults.style.marginBottom = '10px';
        resultsTable.parentNode.insertBefore(downloadBtnResults, resultsTable);

        downloadBtnResults.addEventListener('click', () => {
            let csv = [];
            // Get headers
            const thead = resultsTable.querySelector('thead');
            if (thead) {
                const headers = Array.from(thead.querySelectorAll('th')).map(th => '"' + th.textContent.trim() + '"');
                csv.push(headers.join(';'));
            }
            // Get rows
            const rows = resultsTable.querySelectorAll('tbody tr');
            rows.forEach(row => {
                const cols = Array.from(row.children).map((td, idx, arr) => {
                    let text = td.textContent.trim();
                    return '"' + text + '"';
                });
                csv.push(cols.join(';'));
            });
            // Download
            const csvContent = csv.join('\r\n');
            const blob = new Blob([csvContent], { type: 'text/csv' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'results_table.csv';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
        });
    }

    // INITIALIZE
    showStage('stage1');
    setupTournament();
    
    // Generate button
    document.getElementById('generate-button').addEventListener('click', generateTournament);

    // STAGE 1
    function setupTournament() {
        N_PLAYERS_PER_TEAM = parseInt(setupForm.elements['players-per-team'].value);
        N_ROUNDS = parseInt(setupForm.elements['rounds'].value);
        participants = setupForm.elements['participants'].value.split('\n').map(name => ({ Name: name.trim(), scores: [], finalScore: 0, category: 1 }));
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
        
        // Count duplicate names
        const nameCounts = {};
        for (const participant of participants) {
            const trimmedName = participant.Name.trim();
            nameCounts[trimmedName] = (nameCounts[trimmedName] || 0) + 1;
        }
        const duplicateCount = Object.values(nameCounts).filter(count => count > 1).length;
        analysisDuplicateNames.textContent = duplicateCount;
        analysisDuplicateNames.style.color = duplicateCount === 0 ? 'green' : 'red';

        // Invalidate any previous draft
        gameDraft = null;
        gameScores = null;
        gameCourts = null;

        // Disable stage buttons until Generate is clicked
        stageButtons.stage2.disabled = true;
        stageButtons.stage3.disabled = true;

        // Clear tables
        gamesTableBody.innerHTML = '';
        const winnersTBody = document.querySelector('#results-table tbody');
        if (winnersTBody) winnersTBody.innerHTML = '';
        const chartContainer = document.getElementById('score-chart');
        if (chartContainer) chartContainer.innerHTML = '<div class="score-chart-empty">No scores yet</div>';
    }

    function generateTournament() {
        // Re-read form values to ensure latest data
        N_PLAYERS_PER_TEAM = parseInt(setupForm.elements['players-per-team'].value);
        N_ROUNDS = parseInt(setupForm.elements['rounds'].value);
        participants = setupForm.elements['participants'].value.split('\n').map(name => ({ Name: name.trim(), scores: [], finalScore: 0, category: 1 }));
        if (participants.length <= 1) { return; }
        
        Nplayers = participants.length;
        NgamesPerRound = Math.floor(Nplayers/N_PLAYERS_PER_TEAM/2);
        NteamsPerRound = NgamesPerRound * 2;
        NplayersPerRound = NteamsPerRound * N_PLAYERS_PER_TEAM;
        Ngames = NgamesPerRound * N_ROUNDS;

        const tournamentFormat = setupForm.elements['tournament-format'].value;
        if (tournamentFormat === '2' || tournamentFormat === '3') {
            // Swiss with optimized pairings
            initPairingHistory();
            gameDraft = [];
            const skipSameTeam = tournamentFormat === '3';
            for (let i = 0; i < N_ROUNDS; i++) {
                const roundDraft = generateRoundOptimized(i, skipSameTeam);
                gameDraft.push(roundDraft);
                updatePairingHistory(roundDraft);
            }
        } else {
            // Swiss with random pairings (default)
            gameDraft = Array.from({ length: N_ROUNDS }, (_, i) => generateRoundRandom(i));
            // Build pairing history for display
            initPairingHistory();
            for (let i = 0; i < N_ROUNDS; i++) {
                updatePairingHistory(gameDraft[i]);
            }
        }
        gameScores = Array.from({ length: N_ROUNDS }, () => Array(NteamsPerRound).fill(null));
        gameCourts = Array.from({ length: N_ROUNDS }, () => Array(NgamesPerRound).fill(null));

        populateGamesTable();
        populateWinnersTable();

        // Enable stage buttons now that a draft exists
        stageButtons.stage2.disabled = false;
        stageButtons.stage3.disabled = false;
    }

    function generateRoundRandom(round) {
        return generateUniqueRandomIntegers(NteamsPerRound * N_PLAYERS_PER_TEAM, Nplayers - 1);
    }

    // Pairing history tracking for optimized pairings
    function initPairingHistory() {
        pairingHistory = {
            sameGame: Array.from({ length: Nplayers }, () => new Map()),
            sameTeam: Array.from({ length: Nplayers }, () => new Map())
        };
    }

    function updatePairingHistory(roundDraft) {
        for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
            const team1 = roundDraft.slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
            const team2 = roundDraft.slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);

            // Record same-game pairs (all players in this game)
            const allPlayers = [...team1, ...team2];
            for (let i = 0; i < allPlayers.length; i++) {
                for (let j = i + 1; j < allPlayers.length; j++) {
                    const a = allPlayers[i], b = allPlayers[j];
                    pairingHistory.sameGame[a].set(b, (pairingHistory.sameGame[a].get(b) || 0) + 1);
                    pairingHistory.sameGame[b].set(a, (pairingHistory.sameGame[b].get(a) || 0) + 1);
                }
            }

            // Record same-team pairs (within each team)
            for (const team of [team1, team2]) {
                for (let i = 0; i < team.length; i++) {
                    for (let j = i + 1; j < team.length; j++) {
                        const a = team[i], b = team[j];
                        pairingHistory.sameTeam[a].set(b, (pairingHistory.sameTeam[a].get(b) || 0) + 1);
                        pairingHistory.sameTeam[b].set(a, (pairingHistory.sameTeam[b].get(a) || 0) + 1);
                    }
                }
            }
        }
    }

    function updatePairingHistoryFromTeams(team1Ids, team2Ids) {
        if (!pairingHistory) return;

        // Same-game pairs (all players in this match)
        const allPlayers = [...team1Ids, ...team2Ids];
        for (let i = 0; i < allPlayers.length; i++) {
            for (let j = i + 1; j < allPlayers.length; j++) {
                const a = allPlayers[i], b = allPlayers[j];
                pairingHistory.sameGame[a].set(b, (pairingHistory.sameGame[a].get(b) || 0) + 1);
                pairingHistory.sameGame[b].set(a, (pairingHistory.sameGame[b].get(a) || 0) + 1);
            }
        }

        // Same-team pairs (within each team)
        for (const team of [team1Ids, team2Ids]) {
            for (let i = 0; i < team.length; i++) {
                for (let j = i + 1; j < team.length; j++) {
                    const a = team[i], b = team[j];
                    pairingHistory.sameTeam[a].set(b, (pairingHistory.sameTeam[a].get(b) || 0) + 1);
                    pairingHistory.sameTeam[b].set(a, (pairingHistory.sameTeam[b].get(a) || 0) + 1);
                }
            }
        }
    }

    function removePairingHistoryForRound(roundDraft) {
        if (!pairingHistory) return;
        for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
            const team1 = roundDraft.slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
            const team2 = roundDraft.slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);

            const allPlayers = [...team1, ...team2];
            for (let i = 0; i < allPlayers.length; i++) {
                for (let j = i + 1; j < allPlayers.length; j++) {
                    const a = allPlayers[i], b = allPlayers[j];
                    const prevA = pairingHistory.sameGame[a].get(b) || 0;
                    if (prevA <= 1) pairingHistory.sameGame[a].delete(b); else pairingHistory.sameGame[a].set(b, prevA - 1);
                    const prevB = pairingHistory.sameGame[b].get(a) || 0;
                    if (prevB <= 1) pairingHistory.sameGame[b].delete(a); else pairingHistory.sameGame[b].set(a, prevB - 1);
                }
            }

            for (const team of [team1, team2]) {
                for (let i = 0; i < team.length; i++) {
                    for (let j = i + 1; j < team.length; j++) {
                        const a = team[i], b = team[j];
                        const prevA = pairingHistory.sameTeam[a].get(b) || 0;
                        if (prevA <= 1) pairingHistory.sameTeam[a].delete(b); else pairingHistory.sameTeam[a].set(b, prevA - 1);
                        const prevB = pairingHistory.sameTeam[b].get(a) || 0;
                        if (prevB <= 1) pairingHistory.sameTeam[b].delete(a); else pairingHistory.sameTeam[b].set(a, prevB - 1);
                    }
                }
            }
        }
    }

    function calculateRoundScore(proposedRound, pairingHistory, skipSameTeam) {
        let score = 0;
        for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
            const team1 = proposedRound.slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
            const team2 = proposedRound.slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);
            const allPlayers = [...team1, ...team2];

            // +5 for each repeated same-game pair
            for (let i = 0; i < allPlayers.length; i++) {
                for (let j = i + 1; j < allPlayers.length; j++) {
                    if (pairingHistory.sameGame[allPlayers[i]].has(allPlayers[j])) {
                        score += 5;
                    }
                }
            }

            // +10 for each repeated same-team pair (skipped for format 3 — teams shuffle dynamically)
            if (!skipSameTeam) {
                for (const team of [team1, team2]) {
                    for (let i = 0; i < team.length; i++) {
                        for (let j = i + 1; j < team.length; j++) {
                            if (pairingHistory.sameTeam[team[i]].has(team[j])) {
                                score += 10;
                            }
                        }
                    }
                }
            }
        }
        return score;
    }

    function generateRoundOptimized(round, skipSameTeam) {
        const TRIALS = 1000;
        let bestRound = null;
        let bestScore = Infinity;

        for (let trial = 0; trial < TRIALS; trial++) {
            const candidate = generateUniqueRandomIntegers(NteamsPerRound * N_PLAYERS_PER_TEAM, Nplayers - 1);
            const trialScore = calculateRoundScore(candidate, pairingHistory, skipSameTeam);
            if (trialScore < bestScore) {
                bestScore = trialScore;
                bestRound = candidate;
            }
        }

        return bestRound;
    }

    // STAGE 2
    function getBusyPlayerIds() {
        const busyPlayerIds = new Set();

        // Check regular games — only ongoing (Playing) matches make players busy
        for (let round = 0; round < N_ROUNDS; round++) {
            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const team1Score = gameScores[round][gameId * 2];
                const court = gameCourts[round][gameId];

                const isOngoing = team1Score === null && court !== null;

                if (isOngoing) {
                    const team1Players = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
                    const team2Players = gameDraft[round].slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);
                    for (const id of team1Players) busyPlayerIds.add(id);
                    for (const id of team2Players) busyPlayerIds.add(id);
                }
            }
        }

        // Check extra matches — only ongoing (Playing) matches make players busy
        for (const match of extraMatches) {
            const isOngoing = match.team1Score === null && match.court !== null;

            if (isOngoing) {
                const team1Names = match.team1.split(',').map(name => name.trim());
                const team2Names = match.team2.split(',').map(name => name.trim());
                const allNames = [...team1Names, ...team2Names];
                for (const name of allNames) {
                    const idx = participants.findIndex(p => p.Name === name);
                    if (idx !== -1) busyPlayerIds.add(idx);
                }
            }
        }

        return busyPlayerIds;
    }

    function populateGamesTable() {
        const colors = ['#66FF66', '#6666FF', '#FFFF66', '#66FFFF', '#FF66FF'];
        gamesTableBody.innerHTML = ''; // Clear previous rows

        for (let round = 0; round < N_ROUNDS; round++) {
            const roundColor = colors[round % colors.length];

            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const completeGameId = round * NgamesPerRound + gameId;
                const team1Players = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
                const team2Players = gameDraft[round].slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);
                const team1Score = gameScores[round][gameId * 2];
                const team2Score = gameScores[round][gameId * 2 + 1];
                const court = gameCourts[round][gameId];

                let score;
                if (team1Score === null) {
                    if (court === null) {
                        const busyPlayerIds = getBusyPlayerIds();
                        const team1PlayerIds = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, (gameId + 0.5) * N_PLAYERS_PER_TEAM * 2);
                        const team2PlayerIds = gameDraft[round].slice((gameId + 0.5) * N_PLAYERS_PER_TEAM * 2, (gameId + 1) * N_PLAYERS_PER_TEAM * 2);
                        const allPlayerIds = [...team1PlayerIds, ...team2PlayerIds];
                        const hasBusyPlayers = allPlayerIds.some(id => busyPlayerIds.has(id));
                        if (hasBusyPlayers) {
                            score = `<span class="introduceResult introduceResult-pending" onclick="window.introduceResult(${round}, ${gameId})" title="Warning: some players of this match are busy">Pending <span class="busy-warning">❌</span></span>`;
                        } else {
                            score = `<span class="introduceResult introduceResult-pending" onclick="window.introduceResult(${round}, ${gameId})">Pending</span>`;
                        }
                    } else {
                        score = `<span class="introduceResult playing-status" onclick="window.introduceResult(${round}, ${gameId})">Playing</span>`;
                    }
                } else {
                    score = `<span class="introduceResult introduceResult-done" onclick="window.introduceResult(${round}, ${gameId})">${team1Score} - ${team2Score}</span>`;
                }

                const row = document.createElement('tr');
                row.style.backgroundColor = roundColor; // Apply round color

                row.innerHTML = `
                    <td>${completeGameId + 1}</td>
                    <td>${round + 1}</td>
                    <td class="court-cell">${court !== null ? court : '-'}</td>
                    <td>${team1Players.map(playerId => participants[playerId].Name).join(', ')}</td>
                    <td>${team2Players.map(playerId => participants[playerId].Name).join(', ')}</td>
                    <td>${score}</td>
                `;

                gamesTableBody.appendChild(row);
            }
        }

        // Add extra matches
        const lastGameId = Ngames;
        for (let i = 0; i < extraMatches.length; i++) {
            const extraMatch = extraMatches[i];
            const completeGameId = lastGameId + i + 1;
            const extraColor = '#E8E8E8'; // Light gray for extra matches

            let score;
            if (extraMatch.team1Score === null) {
                if (extraMatch.court === null) {
                    const busyPlayerIds = getBusyPlayerIds();
                    const team1Names = extraMatch.team1.split(',').map(name => name.trim());
                    const team2Names = extraMatch.team2.split(',').map(name => name.trim());
                    const allNames = [...team1Names, ...team2Names];
                    const hasBusyPlayers = allNames.some(name => {
                        const idx = participants.findIndex(p => p.Name === name);
                        return idx !== -1 && busyPlayerIds.has(idx);
                    });
                    if (hasBusyPlayers) {
                        score = `<span class="introduceResult introduceResult-pending" onclick="window.introduceResultExtra(${i})" title="Warning: some players of this match are busy">Pending <span class="busy-warning">❌</span></span>`;
                    } else {
                        score = `<span class="introduceResult introduceResult-pending" onclick="window.introduceResultExtra(${i})">Pending</span>`;
                    }
                } else {
                    score = `<span class="introduceResult playing-status" onclick="window.introduceResultExtra(${i})">Playing</span>`;
                }
            } else {
                score = `<span class="introduceResult introduceResult-done" onclick="window.introduceResultExtra(${i})">${extraMatch.team1Score} - ${extraMatch.team2Score}</span>`;
            }

            const row = document.createElement('tr');
            row.style.backgroundColor = extraColor;

            row.innerHTML = `
                <td>${completeGameId}</td>
                <td>Extra</td>
                <td class="court-cell">${extraMatch.court !== null ? extraMatch.court : '-'}</td>
                <td>${extraMatch.team1}</td>
                <td>${extraMatch.team2}</td>
                <td>${score}</td>
            `;

            gamesTableBody.appendChild(row);
        }

        addPlayerClickHighlight();
        
        // Restore or set default highlighting
        if (currentHighlightedPlayer) {
            highlightGames(currentHighlightedPlayer);
        } else if (gameDraft[0].length > 0) {
            const firstPlayerName = participants[gameDraft[0][0]].Name;
            highlightGames(firstPlayerName);
        }
    }

    window.editCourt = function(round, gameId) {
        window.introduceResult(round, gameId);
    }

    function refreshIntroduceResultPopup(round, gameId, oldPopup, overlay) {
        // Rebuild the popup content in-place to reflect swapped names
        const initialCourt = gameCourts[round][gameId]; // can be null (none)
        const team1PlayerIds = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM);
        const team2PlayerIds = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM, gameId * N_PLAYERS_PER_TEAM * 2 + 2 * N_PLAYERS_PER_TEAM);

        const rankMap = getPlayerRankMap();
        const team1NamesHtml = team1PlayerIds.map(id => {
            const rank = rankMap.get(id);
            const rankHtml = rank ? `<sup style="font-size:0.65em;color:#888;margin-left:1px;">#${rank}</sup>` : '';
            return `<span class="swap-player player-name-link" data-player="${id}" data-team="0">${participants[id].Name}${rankHtml}</span>`;
        }).join(', ');
        const team2NamesHtml = team2PlayerIds.map(id => {
            const rank = rankMap.get(id);
            const rankHtml = rank ? `<sup style="font-size:0.65em;color:#888;margin-left:1px;">#${rank}</sup>` : '';
            return `<span class="swap-player player-name-link" data-player="${id}" data-team="1">${participants[id].Name}${rankHtml}</span>`;
        }).join(', ');

        oldPopup.innerHTML = `
            <h3>Enter results for game ${gameId}</h3>
            <label>Court:</label>
            <div class="court-stepper">
                <button id="court-minus" class="court-btn">−</button>
                <span id="court-display" class="court-display">${initialCourt !== null ? initialCourt : 'none'}</span>
                <button id="court-plus" class="court-btn">+</button>
            </div>
            <label>Team 1 score:</label>
            <input type="number" id="team1-score" placeholder="Enter score" value="${gameScores[round][gameId * 2] !== null ? gameScores[round][gameId * 2] : ''}" />
            <div style="font-size: 13px; color: #666; margin: 2px 0 10px 0;">${team1NamesHtml}</div>
            <label>Team 2 score:</label>
            <input type="number" id="team2-score" placeholder="Enter score" value="${gameScores[round][gameId * 2 + 1] !== null ? gameScores[round][gameId * 2 + 1] : ''}" />
            <div style="font-size: 13px; color: #666; margin: 2px 0 10px 0;">${team2NamesHtml}</div>
            <div class="button-row">
                <button id="ok-button" class="btn-primary">Update</button>
                <button id="cancel-button" class="btn-danger">Cancel</button>
            </div>
        `;

        // Re-attach event listeners
        attachIntroduceResultEvents(round, gameId, oldPopup, overlay);
    }

    function attachIntroduceResultEvents(round, gameId, popup, overlay) {
        let courtValue = gameCourts[round][gameId]; // can be null (none)
        document.getElementById('court-plus').addEventListener('click', () => {
            if (courtValue === null) {
                courtValue = 1;
            } else {
                courtValue++;
            }
            document.getElementById('court-display').textContent = courtValue === null ? 'none' : courtValue;
        });
        document.getElementById('court-minus').addEventListener('click', () => {
            if (courtValue === null) {
                // already none, do nothing
            } else if (courtValue === 1) {
                courtValue = null;
            } else {
                courtValue--;
            }
            document.getElementById('court-display').textContent = courtValue === null ? 'none' : courtValue;
        });

        document.getElementById('ok-button').addEventListener('click', () => {
            const team1Score = document.getElementById('team1-score').value;
            const team2Score = document.getElementById('team2-score').value;

            document.body.removeChild(popup); document.body.removeChild(overlay);

            gameCourts[round][gameId] = courtValue;
            
            if (team1Score !== '' && team2Score !== '') {
                gameScores[round][gameId * 2] = parseInt(team1Score);
                gameScores[round][gameId * 2 + 1] = parseInt(team2Score);
                assignScores();
            }

            rebalancePendingMatches();
            populateGamesTable();
        });
        document.getElementById('cancel-button').addEventListener('click', () => {
            // Revert any swap that happened
            if (popup._originalRoundDraft) {
                const currentDraft = gameDraft[round];
                let swapped = false;
                for (let i = 0; i < currentDraft.length; i++) {
                    if (currentDraft[i] !== popup._originalRoundDraft[i]) {
                        swapped = true;
                        break;
                    }
                }
                if (swapped) {
                    removePairingHistoryForRound(currentDraft);
                    gameDraft[round] = [...popup._originalRoundDraft];
                    updatePairingHistory(gameDraft[round]);
                }
            }
            document.body.removeChild(popup); document.body.removeChild(overlay);
        });

        // Attach swap click handlers
        document.querySelectorAll('.swap-player').forEach(el => {
            el.addEventListener('click', () => {
                const clickedPlayerId = parseInt(el.dataset.player);
                const teamIndex = parseInt(el.dataset.team); // 0 = team1, 1 = team2
                showSwapPopup(round, gameId, popup, overlay, clickedPlayerId, teamIndex);
            });
        });
    }

    function showSwapPopup(round, gameId, mainPopup, mainOverlay, clickedPlayerId, teamIndex) {
        const team1PlayerIds = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM);
        const team2PlayerIds = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM, gameId * N_PLAYERS_PER_TEAM * 2 + 2 * N_PLAYERS_PER_TEAM);
        const otherTeamIds = teamIndex === 0 ? team2PlayerIds : team1PlayerIds;

        const swapOverlay = document.createElement('div');
        swapOverlay.className = 'modal-overlay';
        swapOverlay.style.zIndex = '1001';
        document.body.appendChild(swapOverlay);

        const swapPopup = document.createElement('div');
        swapPopup.className = 'modal-popup';
        swapPopup.style.zIndex = '1002';
        swapPopup.style.minWidth = '280px';

        const otherTeamOptions = otherTeamIds.map(id =>
            `<div class="swap-option" data-target="${id}">${participants[id].Name}</div>`
        ).join('');

        swapPopup.innerHTML = `
            <h3>Swap ${participants[clickedPlayerId].Name}</h3>
            <p style="margin: 8px 0 14px 0; font-size: 14px; color: #666;">Choose a player from the other team to swap with:</p>
            ${otherTeamOptions}
            <div class="button-row" style="margin-top:14px;">
                <button id="cancel-swap-button" class="btn-danger">Cancel</button>
            </div>
        `;

        document.body.appendChild(swapPopup);

        // Attach swap option clicks
        swapPopup.querySelectorAll('.swap-option').forEach(el => {
            el.addEventListener('click', () => {
                const targetPlayerId = parseInt(el.dataset.target);

                // Perform the swap in gameDraft
                const team1Ids = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM);
                const team2Ids = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM, gameId * N_PLAYERS_PER_TEAM * 2 + 2 * N_PLAYERS_PER_TEAM);

                // Build the full round draft for this round to update pairing history
                const oldRoundDraft = [...gameDraft[round]];

                // Remove old pairing history for this round
                removePairingHistoryForRound(oldRoundDraft);

                // Swap the two players in gameDraft
                const clickedIdx = gameDraft[round].indexOf(clickedPlayerId);
                const targetIdx = gameDraft[round].indexOf(targetPlayerId);
                if (clickedIdx !== -1 && targetIdx !== -1) {
                    gameDraft[round][clickedIdx] = targetPlayerId;
                    gameDraft[round][targetIdx] = clickedPlayerId;
                }

                // Add new pairing history for this round
                updatePairingHistory(gameDraft[round]);

                // Close swap popup
                document.body.removeChild(swapPopup);
                document.body.removeChild(swapOverlay);

                // Refresh the main popup
                refreshIntroduceResultPopup(round, gameId, mainPopup, mainOverlay);
            });
        });

        document.getElementById('cancel-swap-button').addEventListener('click', () => {
            document.body.removeChild(swapPopup);
            document.body.removeChild(swapOverlay);
        });
    }

    window.introduceResult = function(round, gameId) {
        const overlay = document.createElement('div');
        overlay.className = 'modal-overlay';
        document.body.appendChild(overlay);
        
        const popup = document.createElement('div');
        popup.className = 'modal-popup';

        // Save original round draft so we can revert swaps on cancel
        popup._originalRoundDraft = [...gameDraft[round]];
        document.body.appendChild(popup);
        refreshIntroduceResultPopup(round, gameId, popup, overlay);
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

        // Add scores from extra matches
        for (let i = 0; i < extraMatches.length; i++) {
            const extraMatch = extraMatches[i];
            
            // Process team 1
            if (extraMatch.team1Score !== null) {
                const team1Players = extraMatch.team1.split(',').map(name => name.trim());
                for (const playerName of team1Players) {
                    const participant = participants.find(p => p.Name === playerName);
                    if (participant) {
                        participant.scores.push(extraMatch.team1Score);
                    }
                }
            }

            // Process team 2
            if (extraMatch.team2Score !== null) {
                const team2Players = extraMatch.team2.split(',').map(name => name.trim());
                for (const playerName of team2Players) {
                    const participant = participants.find(p => p.Name === playerName);
                    if (participant) {
                        participant.scores.push(extraMatch.team2Score);
                    }
                }
            }
        }
    }

    function getPlayerRankMap() {
        const categories = {};
        participants.forEach((p, index) => {
            const cat = p.category || 1;
            if (!categories[cat]) categories[cat] = [];
            let totalScore = 0, averageScore = 0;
            if (p.scores.length > 0) {
                totalScore = p.scores.reduce((sum, score) => sum + score, 0);
                averageScore = totalScore / p.scores.length;
            }
            categories[cat].push({ index, average: averageScore });
        });
        Object.keys(categories).forEach(cat => {
            categories[cat].sort((a, b) => b.average - a.average);
            categories[cat].forEach((p, i) => p.rank = i + 1);
        });
        const rankMap = new Map();
        Object.values(categories).forEach(arr => arr.forEach(p => rankMap.set(p.index, p.rank)));
        return rankMap;
    }

    function rebalancePendingMatches() {
        const tournamentFormat = setupForm.elements['tournament-format'].value;
        if (tournamentFormat !== '3') return;

        const rankMap = getPlayerRankMap();

        for (let round = 0; round < N_ROUNDS; round++) {
            // Check if any game in this round needs rebalancing
            let needsRebalance = false;
            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const team1Score = gameScores[round][gameId * 2];
                const court = gameCourts[round][gameId];
                if (team1Score === null && court === null) {
                    needsRebalance = true;
                    break;
                }
            }
            if (!needsRebalance) continue;

            // Save old draft before modifying for pairing history update
            const oldDraft = gameDraft[round] ? [...gameDraft[round]] : null;

            // Rebalance each pending game in this round
            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const team1Score = gameScores[round][gameId * 2];
                const court = gameCourts[round][gameId];
                if (team1Score !== null || court !== null) continue;

                const startIdx = gameId * N_PLAYERS_PER_TEAM * 2;
                const allPlayerIds = gameDraft[round].slice(startIdx, startIdx + N_PLAYERS_PER_TEAM * 2);

                // Sort by category rank ascending (1 = best)
                allPlayerIds.sort((a, b) => (rankMap.get(a) || 999) - (rankMap.get(b) || 999));

                const newTeam1 = [];
                const newTeam2 = [];
                let front = 0, back = allPlayerIds.length - 1;
                let team1Turn = true;

                // Distribute: best+worst together on Team 1, then next best+worst on Team 2, alternating
                while (front <= back) {
                    const best = allPlayerIds[front++];
                    const worst = allPlayerIds[back--];
                    if (team1Turn) {
                        newTeam1.push(best, worst);
                    } else {
                        newTeam2.push(best, worst);
                    }
                    team1Turn = !team1Turn;
                }

                // Write back to gameDraft
                for (let i = 0; i < N_PLAYERS_PER_TEAM; i++) {
                    gameDraft[round][startIdx + i] = newTeam1[i];
                    gameDraft[round][startIdx + N_PLAYERS_PER_TEAM + i] = newTeam2[i];
                }
            }

            // Update pairing history: remove old, add new
            if (pairingHistory && oldDraft) {
                removePairingHistoryForRound(oldDraft);
                updatePairingHistory(gameDraft[round]);
            }
        }
    }

    function addPlayerClickHighlight() {
        const rankMap = getPlayerRankMap();
        const rows = document.querySelectorAll('#games-table tbody tr');

        rows.forEach(row => {
            const team1Cell = row.children[3];
            const team2Cell = row.children[4];

            [team1Cell, team2Cell].forEach(cell => {
                const players = cell.textContent.split(', ');
                cell.innerHTML = ''; // Clear the cell content

                players.forEach((player, index) => {
                    const span = document.createElement('span');
                    span.textContent = player;
                    span.className = 'player-name';
                    span.addEventListener('click', () => {
                        removeHighlight();
                        highlightGames(player);
                    });
                    cell.appendChild(span);

                    // Add category rank superscript after the name
                    const playerId = participants.findIndex(p => p.Name === player);
                    if (playerId !== -1) {
                        const rank = rankMap.get(playerId);
                        if (rank) {
                            const sup = document.createElement('sup');
                            sup.textContent = '#' + rank;
                            sup.style.fontSize = '0.65em';
                            sup.style.color = '#888';
                            sup.style.marginLeft = '1px';
                            cell.appendChild(sup);
                        }
                    }

                    if (index < players.length - 1) {
                        cell.appendChild(document.createTextNode(', '));
                    }
                });
            });
        });
    }

    function highlightGames(playerName) {
        currentHighlightedPlayer = playerName;

        // Count total games this player participated in
        const playerId = participants.findIndex(p => p.Name === playerName);
        let totalGames = 0;
        if (playerId !== -1) {
            for (let round = 0; round < N_ROUNDS; round++) {
                if (gameDraft[round].includes(playerId)) totalGames++;
            }
        }
        highlightedPlayerSpan.textContent = `${playerName} (x${totalGames})`;

        // Update plays-with display
        if (playerId !== -1 && pairingHistory) {
            // Same team
            const teamMap = pairingHistory.sameTeam[playerId];
            if (teamMap && teamMap.size > 0) {
                const entries = [];
                for (const [otherId, count] of teamMap) {
                    const name = participants[otherId].Name;
                    entries.push(count > 1 ? `${name} (x${count})` : name);
                }
                playsWithTeamSpan.textContent = [...entries].sort((a, b) => a.localeCompare(b)).join(', ');
            } else {
                playsWithTeamSpan.textContent = '-';
            }

            // Same game
            const gameMap = pairingHistory.sameGame[playerId];
            if (gameMap && gameMap.size > 0) {
                const entries = [];
                for (const [otherId, count] of gameMap) {
                    const name = participants[otherId].Name;
                    entries.push(count > 1 ? `${name} (x${count})` : name);
                }
                playsWithGameSpan.textContent = [...entries].sort((a, b) => a.localeCompare(b)).join(', ');
            } else {
                playsWithGameSpan.textContent = '-';
            }
        } else {
            playsWithTeamSpan.textContent = '-';
            playsWithGameSpan.textContent = '-';
        }

        const rows = document.querySelectorAll('#games-table tbody tr');
        rows.forEach(row => {
            const team1Players = row.children[3].querySelectorAll('span');
            const team2Players = row.children[4].querySelectorAll('span');

            const team1Names = Array.from(team1Players).map(span => span.textContent);
            const team2Names = Array.from(team2Players).map(span => span.textContent);

            if (team1Names.includes(playerName) || team2Names.includes(playerName)) {
                row.classList.add('highlighted-row');
            } else {
                row.classList.remove('highlighted-row');
            }
        });
    }

    function removeHighlight() {
        const rows = document.querySelectorAll('#games-table tbody tr');
        rows.forEach(row => {
            row.classList.remove('highlighted-row');
        });
    }

    window.showAddMatchDialog = function() {
        const overlay = document.createElement('div');
        overlay.className = 'modal-overlay';
        document.body.appendChild(overlay);
        
        const popup = document.createElement('div');
        popup.className = 'modal-popup';
        popup.style.minWidth = '500px';

        const sortedParticipants = participants
            .map((p, i) => ({ id: i, name: p.Name }))
            .sort((a, b) => a.name.localeCompare(b.name));
        const optionsHtml = sortedParticipants.map(({id, name}) => `<option value="${id}">${name}</option>`).join('');

        popup.innerHTML = `
            <h3>Add Extra Match</h3>
            <div style="display: flex; gap: 20px; margin-bottom: 15px;">
                <div style="flex: 1;">
                    <strong>Team 1</strong>
                    <div id="team1-players" class="team-players-box"><em>No players selected</em></div>
                    <select id="team1-select" style="width: 100%; margin-bottom: 4px;">
                        <option value="">-- Select player --</option>
                        ${optionsHtml}
                    </select>
                    <span id="team1-count" style="font-size:13px;color:#888;">0 / ${N_PLAYERS_PER_TEAM}</span>
                </div>
                <div style="flex: 1;">
                    <strong>Team 2</strong>
                    <div id="team2-players" class="team-players-box"><em>No players selected</em></div>
                    <select id="team2-select" style="width: 100%; margin-bottom: 4px;">
                        <option value="">-- Select player --</option>
                        ${optionsHtml}
                    </select>
                    <span id="team2-count" style="font-size:13px;color:#888;">0 / ${N_PLAYERS_PER_TEAM}</span>
                </div>
            </div>
            <div class="button-row">
                <button id="add-ok-button" disabled class="btn-primary">Add</button>
                <button id="add-cancel-button" class="btn-danger">Cancel</button>
            </div>
        `;

        document.body.appendChild(popup);

        const team1Selected = [];
        const team2Selected = [];

        function removeOptionFromSelects(playerId) {
            const s1 = document.getElementById('team1-select');
            const s2 = document.getElementById('team2-select');
            const opt1 = s1.querySelector(`option[value="${playerId}"]`);
            const opt2 = s2.querySelector(`option[value="${playerId}"]`);
            if (opt1) opt1.remove();
            if (opt2) opt2.remove();
        }

        function updateTeamDisplay(teamNum) {
            const selected = teamNum === 1 ? team1Selected : team2Selected;
            const playersDiv = document.getElementById(`team${teamNum}-players`);
            const select = document.getElementById(`team${teamNum}-select`);
            const countSpan = document.getElementById(`team${teamNum}-count`);
            const full = selected.length >= N_PLAYERS_PER_TEAM;

            playersDiv.innerHTML = selected.length > 0
                ? selected.map(id => participants[id].Name).join(', ')
                : '<em>No players selected</em>';

            countSpan.textContent = `${selected.length} / ${N_PLAYERS_PER_TEAM}`;
            select.disabled = full;
            select.style.opacity = full ? '0.5' : '1';

            document.getElementById('add-ok-button').disabled =
                team1Selected.length < N_PLAYERS_PER_TEAM || team2Selected.length < N_PLAYERS_PER_TEAM;
        }

        function addPlayerToTeam(teamNum) {
            const select = document.getElementById(`team${teamNum}-select`);
            const playerId = parseInt(select.value);
            if (isNaN(playerId)) return;

            const selected = teamNum === 1 ? team1Selected : team2Selected;
            const otherSelected = teamNum === 1 ? team2Selected : team1Selected;

            if (selected.includes(playerId) || otherSelected.includes(playerId)) return;

            selected.push(playerId);
            removeOptionFromSelects(playerId);
            select.value = '';
            updateTeamDisplay(teamNum);
        }

        document.getElementById('team1-select').addEventListener('change', function() { addPlayerToTeam(1); });
        document.getElementById('team2-select').addEventListener('change', function() { addPlayerToTeam(2); });

        document.getElementById('add-ok-button').addEventListener('click', () => {
            const team1 = team1Selected.map(id => participants[id].Name).join(', ');
            const team2 = team2Selected.map(id => participants[id].Name).join(', ');

            document.body.removeChild(popup);
            document.body.removeChild(overlay);

            extraMatches.push({
                team1: team1,
                team2: team2,
                court: null,
                team1Score: null,
                team2Score: null
            });

            // Update pairing history with the new match
            if (pairingHistory) {
                updatePairingHistoryFromTeams(team1Selected, team2Selected);
            }

            populateGamesTable();

            // Refresh the plays-with display if a player is highlighted
            if (currentHighlightedPlayer) {
                highlightGames(currentHighlightedPlayer);
            }
        });

        document.getElementById('add-cancel-button').addEventListener('click', () => {
            document.body.removeChild(popup);
            document.body.removeChild(overlay);
        });
    };

    window.editCourtExtra = function(matchIndex) {
        window.introduceResultExtra(matchIndex);
    };

    window.introduceResultExtra = function(matchIndex) {
        const match = extraMatches[matchIndex];
        
        const overlay = document.createElement('div');
        overlay.className = 'modal-overlay';
        document.body.appendChild(overlay);
        
        const popup = document.createElement('div');
        popup.className = 'modal-popup';

        const initialCourt = match.court; // can be null (none)

        // Get player names for each team
        const team1Names = match.team1;
        const team2Names = match.team2;

        popup.innerHTML = `
            <h3>Enter Details for Extra Match:</h3>
            <label>Court:</label>
            <div class="court-stepper">
                <button id="court-minus" class="court-btn">−</button>
                <span id="court-display" class="court-display">${initialCourt !== null ? initialCourt : 'none'}</span>
                <button id="court-plus" class="court-btn">+</button>
            </div>
            <label>Team 1 score:</label>
            <input type="number" id="team1-score" placeholder="Enter score" />
            <div style="font-size: 13px; color: #666; margin: 2px 0 10px 0;">${team1Names}</div>
            <label>Team 2 score:</label>
            <input type="number" id="team2-score" placeholder="Enter score" />
            <div style="font-size: 13px; color: #666; margin: 2px 0 10px 0;">${team2Names}</div>
            <div class="button-row">
                <button id="ok-button" class="btn-primary">Update</button>
                <button id="cancel-button" class="btn-danger">Cancel</button>
            </div>
        `;

        document.body.appendChild(popup);

        let courtValue = initialCourt; // can be null (none)
        document.getElementById('court-plus').addEventListener('click', () => {
            if (courtValue === null) {
                courtValue = 1;
            } else {
                courtValue++;
            }
            document.getElementById('court-display').textContent = courtValue === null ? 'none' : courtValue;
        });
        document.getElementById('court-minus').addEventListener('click', () => {
            if (courtValue === null) {
                // already none, do nothing
            } else if (courtValue === 1) {
                courtValue = null;
            } else {
                courtValue--;
            }
            document.getElementById('court-display').textContent = courtValue === null ? 'none' : courtValue;
        });

        document.getElementById('ok-button').addEventListener('click', () => {
            const team1Score = document.getElementById('team1-score').value;
            const team2Score = document.getElementById('team2-score').value;

            document.body.removeChild(popup);
            document.body.removeChild(overlay);

            extraMatches[matchIndex].court = courtValue;

            if (team1Score !== '' && team2Score !== '') {
                extraMatches[matchIndex].team1Score = parseInt(team1Score);
                extraMatches[matchIndex].team2Score = parseInt(team2Score);
                assignScores();
            }

            rebalancePendingMatches();
            populateGamesTable();
        });

        document.getElementById('cancel-button').addEventListener('click', () => {
            document.body.removeChild(popup);
            document.body.removeChild(overlay);
        });
    };

    // STAGE 3
    window.editPlayer = function(playerIndex) {
        const participant = participants[playerIndex];

        const overlay = document.createElement('div');
        overlay.className = 'modal-overlay';
        document.body.appendChild(overlay);

        const popup = document.createElement('div');
        popup.className = 'modal-popup';
        popup.innerHTML = `
            <h3>Edit Player</h3>
            <label>Name:</label>
            <input type="text" id="edit-name" value="${participant.Name}" style="width:100%;padding:10px 14px;border:2px solid #e0e0e0;border-radius:8px;font-size:15px;font-family:inherit;" />
            <label>Category:</label>
            <div class="court-stepper" style="margin-bottom:12px;">
                <button id="category-minus" class="court-btn" style="font-size:28px;width:60px;height:60px;">−</button>
                <span id="category-display" class="court-display" style="font-size:28px;min-width:80px;">${participant.category || 1}</span>
                <button id="category-plus" class="court-btn" style="font-size:28px;width:60px;height:60px;">+</button>
            </div>
            <div class="button-row" style="margin-top:16px;">
                <button id="ok-button" class="btn-primary">Update</button>
                <button id="cancel-button" class="btn-danger">Cancel</button>
            </div>
        `;
        document.body.appendChild(popup);

        let categoryValue = participant.category || 1;
        document.getElementById('category-plus').addEventListener('click', () => {
            categoryValue++;
            document.getElementById('category-display').textContent = categoryValue;
        });
        document.getElementById('category-minus').addEventListener('click', () => {
            if (categoryValue > 1) categoryValue--;
            document.getElementById('category-display').textContent = categoryValue;
        });

        document.getElementById('ok-button').addEventListener('click', () => {
            const newName = document.getElementById('edit-name').value.trim();
            const newCategory = categoryValue;

            if (newName) {
                // If the highlighted player was renamed, update the highlight tracking
                if (currentHighlightedPlayer === participant.Name) {
                    currentHighlightedPlayer = newName;
                }
                participant.Name = newName;
            }
            participant.category = newCategory;

            document.body.removeChild(popup);
            document.body.removeChild(overlay);

            populateWinnersTable();
            populateGamesTable();
        });

        document.getElementById('cancel-button').addEventListener('click', () => {
            document.body.removeChild(popup);
            document.body.removeChild(overlay);
        });
    };

    function populateWinnersTable() {
        winnersTableBody.innerHTML = ''; // Clear previous rows

        // Build player data and group by category to compute per-category ranks
        const categories = {};
        const allPlayers = [];
        participants.forEach((p, index) => {
            const cat = p.category || 1;
            if (!categories[cat]) categories[cat] = [];

            let totalScore, averageScore;
            if (p.scores.length === 0) {
                totalScore = 0;
                averageScore = 0;
            } else {
                totalScore = p.scores.reduce((sum, score) => sum + score, 0);
                averageScore = totalScore / p.scores.length;
            }

            const playerData = {
                Name: p.Name,
                Results: p.scores,
                GamesPlayed: p.scores.length,
                Total: totalScore,
                Average: averageScore,
                index: index,
                category: cat
            };
            categories[cat].push(playerData);
            allPlayers.push(playerData);
        });

        // Assign per-category ranks (sort by average within each category)
        Object.keys(categories).forEach(cat => {
            categories[cat].sort((a, b) => b.Average - a.Average);
            categories[cat].forEach((p, i) => p._catRank = i + 1);
        });

        // Sort all players globally by average descending
        allPlayers.sort((a, b) => b.Average - a.Average);

        // Render all players in global average order, showing per-category rank
        allPlayers.forEach((participant) => {
            const rank = participant._catRank;
            const cat = participant.category;
            const row = document.createElement('tr');
            row.innerHTML = `
                <td>${rank}</td>
                <td>${cat}</td>
                <td><span class="player-name" onclick="window.editPlayer(${participant.index})">${participant.Name}</span></td>
                <td>${participant.Results.join(', ')}</td>
                <td>${participant.GamesPlayed}</td>
                <td>${participant.Total}</td>
                <td>${participant.Average}</td>
            `;
            if (rank === 1) {
                row.classList.add('rank-1');
            } else if (rank === 2) {
                row.classList.add('rank-2');
            } else if (rank === 3) {
                row.classList.add('rank-3');
            }
            winnersTableBody.appendChild(row);
        });

        renderLuckInfo();
        renderScoreChart();
    }

    function renderLuckInfo() {
        const luckContainer = document.getElementById('luck-info');
        if (!luckContainer) return;

        if (!gameDraft || !gameScores) {
            luckContainer.innerHTML = '';
            return;
        }

        const rankMap = getPlayerRankMap();
        // playerLuck[i] = { index, totalLuck, matches }
        const playerLuck = participants.map((p, i) => ({ index: i, name: p.Name, totalLuck: 0, matches: 0 }));

        // Helper: compute luck for a match
        function addMatchLuck(team1Players, team2Players) {
            const team1Ranks = team1Players.map(id => rankMap.get(id) || participants.length);
            const team2Ranks = team2Players.map(id => rankMap.get(id) || participants.length);
            const avgTeam1Rank = team1Ranks.reduce((a, b) => a + b, 0) / team1Ranks.length;
            const avgTeam2Rank = team2Ranks.reduce((a, b) => a + b, 0) / team2Ranks.length;

            for (const playerId of team1Players) {
                const teammateRanks = team1Players.filter(id => id !== playerId).map(id => rankMap.get(id) || participants.length);
                const avgTeammate = teammateRanks.length > 0 ? teammateRanks.reduce((a, b) => a + b, 0) / teammateRanks.length : 0;
                playerLuck[playerId].totalLuck += avgTeam2Rank - avgTeammate;
                playerLuck[playerId].matches++;
            }
            for (const playerId of team2Players) {
                const teammateRanks = team2Players.filter(id => id !== playerId).map(id => rankMap.get(id) || participants.length);
                const avgTeammate = teammateRanks.length > 0 ? teammateRanks.reduce((a, b) => a + b, 0) / teammateRanks.length : 0;
                playerLuck[playerId].totalLuck += avgTeam1Rank - avgTeammate;
                playerLuck[playerId].matches++;
            }
        }

        // Regular games
        for (let round = 0; round < N_ROUNDS; round++) {
            for (let gameId = 0; gameId < NgamesPerRound; gameId++) {
                const team1Score = gameScores[round][gameId * 2];
                const team2Score = gameScores[round][gameId * 2 + 1];
                if (team1Score === null || team2Score === null) continue;

                const team1Players = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2, gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM);
                const team2Players = gameDraft[round].slice(gameId * N_PLAYERS_PER_TEAM * 2 + N_PLAYERS_PER_TEAM, gameId * N_PLAYERS_PER_TEAM * 2 + 2 * N_PLAYERS_PER_TEAM);
                addMatchLuck(team1Players, team2Players);
            }
        }

        // Extra matches
        for (const match of extraMatches) {
            if (match.team1Score === null || match.team2Score === null) continue;
            addMatchLuck(match.team1, match.team2);
        }

        // Compute average luck per player
        const avgLuck = playerLuck
            .filter(p => p.matches > 0)
            .map(p => ({ name: p.name, avg: p.totalLuck / p.matches }));

        if (avgLuck.length === 0) {
            luckContainer.innerHTML = '';
            return;
        }

        avgLuck.sort((a, b) => b.avg - a.avg);
        const luckiest = avgLuck[0];
        const unluckiest = avgLuck[avgLuck.length - 1];

        luckContainer.innerHTML = `
            <p>🍀 Luckiest player: <strong class="luck-lucky">${luckiest.name}</strong></p>
            <p>😩 Unluckiest player: <strong class="luck-unlucky">${unluckiest.name}</strong></p>
        `;
    }

    function renderScoreChart() {
        const chartContainer = document.getElementById('score-chart');
        if (!chartContainer) return;

        // Collect all non-null scores
        if (!gameScores) {
            chartContainer.innerHTML = '<div class="score-chart-empty">No scores yet</div>';
            return;
        }

        const freq = new Map();
        for (const round of gameScores) {
            for (const score of round) {
                if (score !== null && score !== undefined) {
                    freq.set(score, (freq.get(score) || 0) + 1);
                }
            }
        }

        if (freq.size === 0) {
            chartContainer.innerHTML = '<div class="score-chart-empty">No scores yet</div>';
            return;
        }

        // Sort scores ascending
        const sortedScores = Array.from(freq.entries()).sort((a, b) => a[0] - b[0]);
        const minScore = sortedScores[0][0];
        const maxScore = sortedScores[sortedScores.length - 1][0];
        const maxCount = Math.max(...sortedScores.map(([_, count]) => count));
        // Use a safe max height that fits inside the chart container (min-height 180px
        // minus padding, x-label, and score labels beneath bars)
        const MAX_BAR_HEIGHT = 250;

        let barsHtml = '';
        for (let score = minScore; score <= maxScore; score++) {
            const count = freq.get(score) || 0;
            const barHeight = count === 0 ? 4 : Math.max(4, (count / maxCount) * MAX_BAR_HEIGHT);
            barsHtml += `
                <div class="score-chart-wrapper">
                    <div class="score-chart-bar" style="height:${barHeight}px;"></div>
                    <div class="score-chart-label">${score}</div>
                </div>
            `;
        }
        chartContainer.innerHTML = `
            <div class="score-chart-y-label">Frequency</div>
            <div class="score-chart-bars-area">
                <div class="score-chart-bars-row">${barsHtml}</div>
                <div class="score-chart-x-label">Scores</div>
            </div>
        `;
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