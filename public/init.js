'use strict';

var $el = document.getElementById('app-root');

var ws = new WebSocket("ws://localhost:3000/stream");

ws.onopen = function() {
    // Web Socket is connected, send data using send()
    // ws.send("Message to send");
};

ws.onmessage = function (evt) {
    var msg = evt.data;
    console.log(msg);
};

ws.onclose = function() {
    // websocket is closed.
    alert("Connection is closed...");
};

Elm.Main.init({
    node: $el
});
