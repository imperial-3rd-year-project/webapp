const SOCKET_ERR = function (event, state) {
    return event.data.toString().startsWith("ERR");
};

const SOCKET_BEGIN_DEVICES = function (event, state) {
    return event.data.toString() === "BEGIN DEVLIST";
};

const SOCKET_END_DEVICES = function (event, state) {
    return event.data.toString() === "END DEVLIST";
};

const SOCKET_RECEIVE_DEVICE = function (event, state) {
    return state.fetchingDevices;
};

const SOCKET_BEGIN_YOLO = function (event, state) {
    return event.data.toString() === "BEGIN YOLO";
};

const SOCKET_END_YOLO = function (event, state) {
    return event.data.toString() === "END YOLO";
};

const SOCKET_RECEIVE_YOLO = function (event, state) {
    return state.fetchingYolo;
};

const SOCKET_RECEIVE_RESNET = function (event, state) {
    return event.data.toString().startsWith("RESNET ");
};

const SOCKET_RECEIVE_WEBCAM_FRAME = function (event, state) {
    return true;
};

const SOCKET_END_GS = function (event, state) {
    return event.data.toString() === "END GS";
};

let handlers = {};
let state = {};
let socket;

function initialiseSocket(pageState) {
    // Create WebSocket connection.
    socket = new WebSocket('ws://127.0.0.1:9160');
    socket.binaryType = 'arraybuffer';
    
    let greeting = "HELLO";
    if (pageState.hasOwnProperty("devices")) {
        greeting = "WEBCAM";
    }
    
    socket.addEventListener('open', function (event) {
        socket.send(greeting);
    });
    
    handlers[SOCKET_ERR] = handleErr;
    state = pageState;
}

let priorityChain = [SOCKET_ERR];

function addSocketHandler(event, handler) {
    handlers[event] = handler;
    priorityChain.push(event);
}

function setSocketHandler() {
    socket.addEventListener('message', function(event) {
        for (let matcher of priorityChain) {
            if (matcher(event, state)) {
                handlers[matcher](event, state);
                break;
            }
        }
    });
}

const WEBCAM = {
    OPEN: "OPEN",
    CLOSE: "CLOSE"
};

const CAPTURE = {
    YOLO: "CAPTURE YOLO",
    RESNET: "CAPTURE RESNET",
    GREENSCREEN: "CAPTURE GS"
};

const IMAGE = {
    YOLO: "IMAGE YOLO",
    RESNET: "IMAGE RESNET",
    SUPERRES: "IMAGE SUPERRES",
    GREENSCREEN: "IMAGE GS",
};

function sendMsg(msg, data) {
    let str = msg;
    if (data != null)  {
        str += ' ' + data;
    }
    socket.send(str);
}

function sendData(data) {
    socket.send(data);
}

// Each function returns true to indicate that it has consumed the event
// and false to let the event propagate down the chain

function handleErr(event) {
    if (event.data.toString().startsWith("ERR")) {
        console.log(event.data);
        return true;
    }

    return false;
}
