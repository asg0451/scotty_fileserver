///
'use strict';

var getTorrentData = function () {
    var x = new XMLHttpRequest();
    x.addEventListener("load", listener);
    x.open("GET", "/torrentstatusraw", true);
    x.send();
};
var listener = function () {
    refresh(this.response);
};
var intrvl = setInterval(getTorrentData, 1000);

var refresh = function (dataEscaped) {
    var data = JSON.parse(JSON.parse(dataEscaped));
    console.log(data);
    // initialize
    var table = document.getElementById("res");
    table.innerHTML = "";
    var head = table.insertRow(0);
    headRow.forEach( c => {
        var r = head.insertCell();
        r.innerHTML = c;
    });
    //////////////
    var torrents = data.arguments.torrents;
    console.log(torrents);
    torrents.forEach( t => {
        console.log(t);
        var r = table.insertRow();
        var c = r.insertCell();
        c.innerHTML = t.id;

        c = r.insertCell();
        c.innerHTML = Math.round(((t.sizeWhenDone - t.leftUntilDone)/t.sizeWhenDone)*100) + "%";

        c = r.insertCell();
        c.innerHTML = sizeify(t.sizeWhenDone - t.leftUntilDone);

        c = r.insertCell();
        c.innerHTML = etaLookup(t.eta);

        c = r.insertCell();
        c.innerHTML = t.rateUpload / 1000 + " kB/s";

        c = r.insertCell();
        c.innerHTML = t.rateDownload / 1000 + " kB/s";

        c = r.insertCell();
        c.innerHTML = t.uploadRatio < 0 ? "N/A" : t.uploadRatio;

        c = r.insertCell();
        c.innerHTML = statusLookup(t.status);

        c = r.insertCell();
        c.innerHTML = t.name;

        var button = document.createElement("input");
        button.type = "button";
        button.value = "Remove";
        button.className = "btn btn-danger";
        curId = t.id;
        console.log(curId);
        button.onclick = removeTorrent;

        c = r.insertCell();
        c.appendChild(button);
    });
};

/////////// vars

var curId = -1;

var headRow = [  "ID",
                 "Done",
                 "Have",
                 "ETA",
                 "Up",
                 "Down",
                 "Ratio",
                 "Status",
                 "Name"  ];

////////// xhttp funcs

var removeTorrent = function() {
    var x = new XMLHttpRequest();
    x.open("POST", "/removetorrent", true);
    x.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    x.addEventListener("load", function() { curId = -1; });
    x.send("torrentid=" + encodeURIComponent(curId));
};


/////////// utility functions

var statusLookup = function (s) {
    switch(s) {
    case 0:
        return "Stopped";
    case 4:
        return "Going On";
    case 6:
        return "Seeding";
    default:
        return "idk";
    }
};
var etaLookup = function (e) {
    switch(e) {
    case -1:
        return "Stopped";
    case -2:
        return "Unknown";
    default:
        return fmt(e);
    }
};

var fmt = function (s) {
    var min = Math.floor(s/60);
    var sec = s - min * 60;
    var hrs = Math.floor(s/3600);
    if (hrs < 1)
        return str_pad_left(min,'0',2) + ":" + str_pad_left(sec,'0',2);
    else
        return str_pad_left(hrs, '0', 2) + ":" + str_pad_left(min,'0',2) + ":" + str_pad_left(sec,'0',2);
};

function str_pad_left(string,pad,length) {
    return (new Array(length+1).join(pad)+string).slice(-length);
}

var sizeify = function (n) {
    if (n > 1000000000)
        return Math.round(n/1000000000) + " GB";
    else if (n > 1000000)
        return Math.round(n/1000000) + " MB";
    else if (n > 1000)
        return Math.round(n/1000) + " KB";
    else return n + " B";
};
