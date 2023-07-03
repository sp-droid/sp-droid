const { app, BrowserWindow, 
  ipcMain, 
  dialog
} = require('electron');
const path = require('path');
const { spawn } = require('child_process');

// Reload on frontend changes
require('electron-reload')(__dirname);

// Handle creating/removing shortcuts on Windows when installing/uninstalling.
if (require('electron-squirrel-startup')) {
  app.quit();
}

const createWindow = () => {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 1920,
    height: 1080,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: true,
      enableRemoteModule: false,
      preload: path.join(__dirname, 'preload.js'),
    },
  });

  // and load the index.html of the app.
  mainWindow.loadFile(path.join(__dirname, 'index.html'));

  // Open the DevTools.
  mainWindow.webContents.openDevTools();

  // Set up an IPC listener to handle the "show-open-dialog" message.
  ipcMain.handle('show-open-dialog', async (event, options) => {
    const result = await dialog.showOpenDialog(mainWindow, options);
    return result;
  });

  //
  let childProcess = null;
  ipcMain.handle('spawn-cmd', async (event) => {
    childProcess = spawn('cmd.exe');
    // Send data from the child process to the renderer process
    childProcess.stdout.on('data', (data) => {
      mainWindow.webContents.send('process-data', data.toString());
    });
    childProcess.stderr.on('data', (data) => {
      mainWindow.webContents.send('process-data', data.toString());
    })
    
  });
  ipcMain.handle('execute-command', async (event, command) => {
    childProcess.stdin.write(command);    
  });
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow);

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and import them here.
