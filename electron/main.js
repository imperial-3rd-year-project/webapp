const { app, BrowserView, BrowserWindow } = require('electron')
const { spawn } = require('child_process');

function createWindow () {
  const mainWindow = new BrowserWindow({ backgroundColor: "#000" });
  const child  = spawn(".stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/web-app/web-app");
  mainWindow.loadURL("http://localhost:3000");
  child.stderr.on('data', (data) => { console.error(`stderr: ${data}`); });
  child.stdout.on('data', (data) => { console.log(`data: ${data}`);});
  mainWindow.on('close', () => child.kill());
}

app.disableHardwareAcceleration();
app.whenReady().then(createWindow)

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
})

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
})

app.on('certificate-error', function(event, webContents, url, error, 
  certificate, callback) {
      event.preventDefault();
      callback(true);
});
