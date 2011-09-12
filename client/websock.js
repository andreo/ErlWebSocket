
function init() {
    var ws = new WebSocket("ws://localhost:8080/aaa/bbb");

    ws.onopen = function() {
        document.write("Opened<br/><br/>");
        ws.send("Hello Server!")
    };
    ws.onmessage = function (e) {
        document.write(e.data + "<br/>");
    };
    ws.onclose = function() {
        document.write("<br/>Closed<br/>");
    };
}
