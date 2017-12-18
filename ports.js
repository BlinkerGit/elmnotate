'use strict'
const Elm = require('./elm.js')

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

// start the elm app in the container
// and keep a reference for communicating with the app
let annotate = Elm.Main.embed(container)

// draw graphics on the canvas
annotate.ports.render.subscribe(function(graphics) {
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
    getImageSize(url, function(imgW, imgH) {
        let updateDims = function() {
            let canvasPanel = document.getElementById('canvas-panel');
            if (canvasPanel) {
                let panelSize = {
                    width: canvasPanel.offsetWidth,
                    height: canvasPanel.offsetHeight,
                };

                annotate.ports.clientDims.send([
                    imgW,
                    imgH,
                    panelSize.width,
                    panelSize.height
                ]);
            } else {
                window.setTimeout(updateDims, 10);
            }
        };

        updateDims();
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
    const canvas = document.getElementById("annotate-canvas");
    if (canvas) {
        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        // HACK: draw something transparent to trigger canvas repaint
        ctx.globalAlpha = 0.01;
        ctx.beginPath();
        ctx.arc(0, 0, 10, 0, tau, false);
        ctx.fillStyle = 'red';
        ctx.fill();

        ctx.globalAlpha = 1.0;
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
