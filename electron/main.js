const { app, BrowserView, BrowserWindow } = require('electron')
const { spawn } = require('child_process');
const urlExist = require("url-exist");
 
function createWindow () {
  const mainWindow = new BrowserWindow();
  mainWindow.maximize();
  mainWindow.loadFile("electron/index.html");
  const child  = spawn(".stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/web-app/web-app");
  child.stderr.on('data', (data) => { console.error(`stderr: ${data}`); });
  child.stdout.on('data', (data) => { console.log(`data: ${data}`);});
  const check = async () => {
      return await urlExist("http://localhost:3000");
  };
  let loadSite = setInterval(() => { 
    let doesExist = check(); 
    doesExist.then((exists) => {
      if (exists === true) { 
        mainWindow.loadURL("http://localhost:3000");
        clearInterval(loadSite);
      } else {
        console.log("Not loaded!");
      }
    });
  }, 100);
  mainWindow.on('close', () => child.kill());
}

app.disableHardwareAcceleration();
app.whenReady().then(createWindow);

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
});

app.on('certificate-error', function(event, webContents, url, error, 
  certificate, callback) {
      event.preventDefault();
      callback(true);
});
