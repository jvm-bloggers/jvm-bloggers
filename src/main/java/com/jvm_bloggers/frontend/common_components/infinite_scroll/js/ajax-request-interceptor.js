(function(XHR) {
    "use strict";

    var send = XHR.prototype.send;

    XHR.prototype.send = function(data) {
        this.setRequestHeader("Wicket-Ajax", "true");
        this.setRequestHeader("Wicket-Ajax-BaseURL", Wicket.Ajax.baseUrl || ".");

        send.call(this, data);
    }
})(XMLHttpRequest);