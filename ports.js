'use strict'
const Elm = require('./elm.js')

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

// start the elm app in the container
// and keep a reference for communicating with the app
let annotate = Elm.Main.embed(container)

let canvas = null;
let ctx = null;

// draw graphics on the canvas
annotate.ports.render.subscribe(function(graphics) {
    console.log("render", graphics);
    clearCanvas();
    graphics.points.forEach(point => {
        drawPoint(point.x, point.y);
    });
    graphics.lines.forEach(line => {
        drawLine(line.start, line.end);
    });
});

// discover the image size and report back to elm
annotate.ports.loadImage.subscribe(function(url) {
    clearCanvas();
    getImageSize(url, function(w, h) {
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

function clearCanvas() {
    console.log("called clear canvas...")
    const canvas = document.getElementById("annotate-canvas");
    if (canvas) {
        const ctx = canvas.getContext('2d');
        console.log(`clearing rect 0, 0 to ${canvas.width}, ${canvas.height}`);
        ctx.clearRect(0, 0, canvas.width, canvas.height);
    }
}

const tau = 2 * Math.PI;

function drawPoint(x, y) {
    const canvas = document.getElementById("annotate-canvas");
    if (canvas) {
        const ctx = canvas.getContext('2d');
        ctx.beginPath();
        ctx.arc(x-1, y-1, 2, 0, tau, false);
        ctx.strokeStyle = 'green';
        //ctx.closePath();
        ctx.stroke();
    }
}

function drawLine(start, end) {
    const canvas = document.getElementById("annotate-canvas");
    if (canvas) {
        const ctx = canvas.getContext('2d');
        ctx.beginPath();
        ctx.moveTo(start.x, start.y);
        ctx.lineTo(end.x, end.y);
        ctx.strokeStyle = 'red';
        //ctx.closePath();
        ctx.stroke();
    }
}
