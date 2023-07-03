// IMPORTS
const { fs } = window.electron;
const { setStdoutListener, spawnCmd, executeCommand } = window.child_process;

// REFERENCES
// MAINPAGE REFERENCES
const configPage = document.getElementById('configPage');
const executePage = document.getElementById('executePage')
const resultsPage = document.getElementById('resultsPage')
// CONFIG PAGE REFERENCES
const jsonPathDiv = document.getElementById('jsonPathDiv');
const jsonDisplay = document.getElementById('jsonDisplay');
const jsonDisplayFont = document.getElementById('jsonDisplayFont');
// EXECUTE PAGE REFERENCES
const consoleDisplay = document.getElementById('consoleDisplay');

// console.log('few')

// VARIABLES
let jsonPath = null;
let jsonData = null;

// LISTENER: CHANGE JSON DISPLAY FONTSIZE
jsonDisplayFont.addEventListener('change', function(event) {
    // Get the selected value
	const selectedValue = jsonDisplayFont.options[jsonDisplayFont.selectedIndex].value;

	// Set the font size of the body element
	jsonDisplay.style.fontSize = selectedValue + "px";
});

// LISTENER: JSON TEXT UPDATE
jsonPathDiv.addEventListener('input', function(event) {
    const parentDiv = event.target.parentNode.parentNode
    
    if (event.target.value.endsWith('.json')) {
        parentDiv.querySelector('#configLoadButton').disabled = false;
        // parentDiv.querySelector('#configCreateButton').disabled = false;
    } else {
        parentDiv.querySelector('#configLoadButton').disabled = true;
        // parentDiv.querySelector('#configCreateButton').disabled = true;
    }
});

// LISTENER: JSON CLICK SEARCH SET
jsonPathDiv.addEventListener('click', function(event) {
    const parentDiv = event.target.parentNode.parentNode
    
    if (event.target.id === 'configPathButton') {
        dialog.showOpenDialog({
            properties: ['openFile'],
        }).then(result => {
            const filePath = result[0].replace(/\\/g, '/');
            
            // Update text field
            parentDiv.querySelector('#configPathText').value = filePath;

            // Enable buttons if the file is .json
            if (filePath.endsWith('.json')) {
                parentDiv.querySelector('#configLoadButton').disabled = false;
                // parentDiv.querySelector('#configCreateButton').disabled = false;
            } else {
                parentDiv.querySelector('#configLoadButton').disabled = true;
                // parentDiv.querySelector('#configCreateButton').disabled = true;
            }
        });
    // Load json file
    } else if (event.target.id === 'configLoadButton') {
        jsonPath = parentDiv.querySelector('#configPathText').value;
        fs.readFile(jsonPath, 'utf-8', (error, data) => {
            if (error) {
              console.error('Error loading JSON file:', error);
            } else {
              jsonData = JSON.parse(data);
              // Update jsonDisplay
              jsonDisplay.textContent = JSON.stringify(jsonData, null, 2)
            }
          });
    }
});

// Functions
setupConsole()
async function setupConsole() {
    await setStdoutListener()
    window.addEventListener('stdoutEvent', (event) => {
        consoleDisplay.value += event.detail;
        consoleDisplay.scrollTop = consoleDisplay.scrollHeight;
    });
    await spawnCmd();
}

// Step pages enable/disables
function enableConfigPage() {
    configPage.style.display = 'block';
    executePage.style.display = 'none';
    resultsPage.style.display = 'none';
}
async function enableExecutePage() {
    configPage.style.display = 'none';
    executePage.style.display = 'block';
    resultsPage.style.display = 'none';

    await executeCommand('conda activate airbus\n');
    await executeCommand('C:\n');
    await executeCommand('cd C:/Users/a1pab/Desktop/Airbus/UPM-A/src\n');
    await executeCommand('index.py -j C:/Users/a1pab/Desktop/Airbus/UPM-A/example/config.json -t run_model basic_stats -s s18-mspSMALL --run\n');
}
function enableResultsPage() {
    configPage.style.display = 'none';
    executePage.style.display = 'none';
    resultsPage.style.display = 'block';

    fs.readdir('C:/Users/a1pab/Desktop/Airbus/UPM-A/example/output', (err, files) => {
        files.forEach(file => {
            if (file.endsWith('.html')) {
                console.log(file);
            }
        });
    });
    // This commented part is a snippet to open links in new browser instead of on the same electron
    // const win = new BrowserWindow();
    // win.webContents.setWindowOpenHandler(({ url }) => {
    //     // config.fileProtocol is my custom file protocol
    //     if (url.startsWith(config.fileProtocol)) {
    //         return { action: 'allow' };
    //     }
    //     // open url in a browser and prevent default
    //     shell.openExternal(url);
    //     return { action: 'deny' };
    // });
}