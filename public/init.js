/* globals Elm document */

(function () {
    "use strict";

    var app = Elm.Main.init({
        flags: {
            sessionId: null
        }
    });

    app.ports.connect.subscribe(function (token) {
        document.cookie = "authorization=" + token;
        var ws = new WebSocket("ws://localhost:3000/stream");

        ws.onmessage = function (evt) {
            var msg = evt.data;
            console.log(msg);
            app.ports.observe_.send(msg);
        };

        ws.onclose = function() {
            // websocket is closed.
            alert("Connection is closed...");
        };
    });
})();
