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
    torrents.forEach( t => {
        var r = table.insertRow();
        r.insertCellContents = function(c) {
            var cell = r.insertCell();
            cell.innerHTML = c;
        };

        fields(t).forEach(c => {r.insertCellContents(c);});

        var button = document.createElement("input");
        button.type = "button";
        button.value = "Remove";
        button.className = "btn btn-danger";
        curId = t.id;
        button.onclick = removeTorrent;

        var c = r.insertCell();
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

var fields = t => {
    return  [ t.id,
              Math.round(((t.sizeWhenDone - t.leftUntilDone)/t.sizeWhenDone)
                         *100) + "%",
              sizeify(t.sizeWhenDone - t.leftUntilDone),
              etaLookup(t.eta),
              t.rateUpload / 1000 + " kB/s",
              t.rateDownload / 1000 + " kB/s",
              t.uploadRatio < 0 ? "N/A" : t.uploadRatio,
              statusLookup(t.status),
              t.name ];
};

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
        return "Going On"; // idle or up and down
    case 6:
        return "Seeding";
    case 2:
        return "Verifying";
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
    var hrs = Math.floor(s/3600);
    var min = Math.floor((s - hrs*3600)/60);
    var sec = s - min * 6 - hrs * 36000;
    return str_pad_left(hrs, '0', 2) + ":" + str_pad_left(min,'0',2) + ":" + str_pad_left(sec,'0',2);

};

var str_pad_left = function (string,pad,length) {
    return (new Array(length+1).join(pad)+string).slice(-length);
};

var sizeify = function (n) {
    if (n > 1000000000)
        return Math.round(n/1000000000) + " GB";
    else if (n > 1000000)
        return Math.round(n/1000000) + " MB";
    else if (n > 1000)
        return Math.round(n/1000) + " KB";
    else return n + " B";
};
