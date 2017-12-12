'use strict'
const Elm = require('./elm.js')

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

// start the elm app in the container
// and keep a reference for communicating with the app
let annotate = Elm.Main.embed(container)

annotate.ports.renderPoints.subscribe(function(points) {
    console.log("renderPoints", points);
});

annotate.ports.renderLines.subscribe(function(points) {
    console.log("renderLines", points);
});

annotate.ports.loadImage.subscribe(function(url) {
    getImageSize(url, function(w, h) {
        //annotate.ports.imageSize.send(`[${w},${h}]`);
        annotate.ports.imageSize.send([w, h]);
    });
});

function getImageSize(url, cb) {
    var image = new Image();
    image.onload = function() {
        cb(image.width, image.height);
    }
    image.src = url;
}
