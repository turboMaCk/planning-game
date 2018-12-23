/* globals Elm document */

(function () {
    "use strict";

    var cookie = {
        read: function (name) {
            var nameEQ = name + "=";
            var ca = document.cookie.split(';');
            for (var i = 0; i < ca.length; i++) {
                var c = ca[i];
                while (c.charAt(0) === ' ') {
                    c = c.substring(1, c.length);
                }

                if (c.indexOf(nameEQ) === 0) {
                    return c.substring(nameEQ.length, c.length);
                }
            }
        },

        write: function (name, value, days) {
            var expires;

            if (days) {
                var date = new Date();
                date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
                expires = "; expires=" + date.toGMTString();
            } else {
                expires = '';
            }

            document.cookie = name + "=" + value + expires + "; path=/";
        },

        erase: function (name) {
            this.write(name, '', -1);
        }
    };

    var app = Elm.Main.init({
        flags: {
            sessionId: cookie.read('sessionId') || null
        }
    });

    // Writing cookie
    app.ports.storeSession.subscribe(function (id) {
        cookie.write('sessionId', id);
    });

    app.ports.connect.subscribe(function (tableId) {
        const host = window.location.host;
        var ws = new WebSocket('ws://' + host + '/tables/' + tableId + '/stream');

        ws.onerror = function () {
            app.ports.streamError.send('can_not_connect');
        };

        ws.onmessage = function (evt) {
            var msg = evt.data;
            console.log("socket", msg);
            app.ports.observe_.send(msg);
        };

        ws.onclose = function() {
            // app.ports.streamError.send('connection_closed');
        };

        app.ports.emit.subscribe(function (msg) {
            ws.send(msg);
        });

        app.ports.disconnect.subscribe(function() {
            if (ws.readyState === WebSocket.OPEN) {
                ws.close(1000);
            }
        });
    });
})();
