
function drawBoundingBoxes(boxes, ctx, offsetX, offsetY) {
  /*
      boxes: [ [label: str, l: int, r: int, t: int, b: int] ]
  */
  for (let box of boxes) {
      ctx.beginPath();
      let x = Math.max(0, parseInt(box[1]));
      let y = Math.max(0, parseInt(box[3]));
      const w = parseInt(box[2]) - x;
      const h = parseInt(box[4]) - y;
      if (offsetX != null && offsetY != null) {
          x += offsetX;
          y += offsetY;
      }
      ctx.rect(x, y, w, h);
      ctx.lineWidth = 4;
      ctx.strokeStyle = 'rgb(255, 0, 0)';
      ctx.stroke();

      ctx.font = '12px monospace';
      ctx.fillStyle = 'rgb(255, 0, 0)';
      let dist = 10;
      if (y - dist < 5) {
          dist = -10;
      }
      ctx.fillText(box[0], x, y - dist);
  }
}

function drawWebcamImage(event, state) {
    let arr = new Uint8Array(event.data);
    let dest = state.webcamCtx.getImageData(0, 0, w, h);
    for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
            const sIndex = w * y + x;
            const dIndex = w * y + (w - x - 1);
            dest.data[dIndex * 4 + 0] = arr[sIndex * 3 + 0];
            dest.data[dIndex * 4 + 1] = arr[sIndex * 3 + 1];
            dest.data[dIndex * 4 + 2] = arr[sIndex * 3 + 2];
        }
    }
    state.webcamCtx.putImageData(dest, 0, 0);
}

// Utility functions for socket communication

let buildState = function (initialState) {
    let state = initialState;
    return {
        withWebcam: function () {
            state.fetchingDevices = false;
            state.devices = [];
            return this;
        },
        withYolo: function () {
            state.fetchingYolo = false;
            return this;
        },
        withResnet: function () {
            state.fetchingResnet = false;
            return this;
        },
        build: function() {
            return state;
        }
    };
}

function onBeginDevices(event, state) {
    state.fetchingDevices = true;
    return true;
}

function onEndDevices(event, state) {
    state.fetchingDevices = false;
    // Set the options
    let devicesSelect = document.getElementById("devices");
    state.devices.reverse();
    for (let device of state.devices) {
        let option = document.createElement("option");
        option.text = device[0] + ": " + device[1];
        console.log(option.text);
        devicesSelect.add(option);
    }
    return true;
}

function onReceiveDevice(event, state) {
    // Parse device type
    const devicePath = event.data.substr(0, event.data.indexOf('$'));
    const deviceName = event.data.substr(event.data.indexOf('$') + 1);
    const details = [devicePath, deviceName];
    state.devices.push(details);
    return true;
}

function onYoloBegin(event, state) {
    state.fetchingYolo = true;
    state.boundingBoxes = [];
    state.detail = 0;
    state.currBox = [];
}

function onYoloReceive(event, state) {
    console.log(event.data);
    state.currBox.push(event.data);
    state.detail++;
    if (state.detail == 5) {
        state.detail = 0;
        state.boundingBoxes.push(state.currBox);
        state.currBox = [];
    }
}