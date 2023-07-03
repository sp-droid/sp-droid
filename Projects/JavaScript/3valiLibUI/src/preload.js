// See the Electron documentation for details on how to use preload scripts:
// https://www.electronjs.org/docs/latest/tutorial/process-model#preload-scripts

const { contextBridge, ipcRenderer } = require('electron');
const fs = require('fs');

// Expose nodeJS file system module
contextBridge.exposeInMainWorld('electron', {
    fs: fs,
});

// Expose the dialog module to the renderer process via the context bridge.
contextBridge.exposeInMainWorld('dialog', {
    showOpenDialog: async (options) => {
        const result = await ipcRenderer.invoke('show-open-dialog', options);
        return result.filePaths;
    },
});

// Expose the child_process module to the renderer process
contextBridge.exposeInMainWorld('child_process', {
    // Set up listener
    setStdoutListener: () => {
        ipcRenderer.on('process-data', (event, data) => {
            const customEvent = new CustomEvent('stdoutEvent', { detail: data });
            window.dispatchEvent(customEvent);
        });
    },
    // Spawn child process
    spawnCmd: async () => {
        await ipcRenderer.invoke('spawn-cmd');
    },
    // Send request to main
    executeCommand: async (command) => {
        await ipcRenderer.invoke('execute-command', command);
    },
});

// ipcRenderer.on('process-data', (event, data) => {
//     console.log(data);
// });