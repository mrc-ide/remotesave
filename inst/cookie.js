Shiny.addCustomMessageHandler("updateCookie", function(message) {
    var getCookieFromString = function(s, name, defaultValue) {
        var m = s.match("(^|; )" + name + "=(.*?)($|; )");
        return (m === null ? defaultValue : m[2]);
    }

    var futureDate = function(days) {
        var date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        return date;
    }

    var updateCookie = function(name, valid, defaultValue) {
        var value = getCookieFromString(document.cookie, name, defaultValue);
        var expires = futureDate(valid).toUTCString();
        document.cookie = name + "=" + value + "; expires=" + expires;
        return value;
    }

    var value = updateCookie(message.name, message.valid, message.defaultValue);
    Shiny.onInputChange(message.input, value);
})

Shiny.addCustomMessageHandler("clearCookie", function(message) {
    document.cookie = message + "= ; expires=Thu, 01 Jan 1970 00:00:00 UTC;"
})
