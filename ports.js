'use strict'
const Elm = require('./elm.js')

// get a reference to the div where we will show our UI
let container = document.getElementById('container')

// start the elm app in the container
// and keep a reference for communicating with the app
let annotate = Elm.Main.embed(container)


function withCanvasContext(cb) {
    const canvas = document.getElementById('annotate-canvas');
    if (canvas) {
        const ctx = canvas.getContext('2d');
        cb(canvas, ctx);
    } else {
        console.log("unable to get reference to canvas!");
    }
}

// draw graphics on the canvas
annotate.ports.render.subscribe(function(graphics) {
    const doRender = function() {
        clearCanvas();
        if (graphics.highlight.length > 0) {
            highlight(graphics.highlight);
        }
        graphics.points.forEach(point => {
            drawPoint(point.x, point.y);
        });
        graphics.lines.forEach(line => {
            drawLine(line.start, line.end);
        });
        graphics.anchors.forEach(point => {
            drawAnchor(point.x, point.y);
        });
    };

    // make render async
    setTimeout(doRender, 100);
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

const tau = 2 * Math.PI;

function clearCanvas() {
    withCanvasContext(function(canvas, ctx) {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        // HACK: draw something transparent to trigger canvas repaint
        ctx.globalAlpha = 0.01;
        ctx.beginPath();
        ctx.arc(0, 0, 10, 0, tau, false);
        ctx.closePath();
        ctx.fillStyle = 'red';
        ctx.fill();

        ctx.globalAlpha = 1.0;
    });
}


function drawPoint(x, y) {
    withCanvasContext(function(canvas, ctx) {
        ctx.beginPath();
        ctx.arc(x-1, y-1, 2, 0, tau, false);
        ctx.closePath();
        ctx.strokeStyle = 'rgb(50,205,50)';
        ctx.stroke();
    });
}

function drawAnchor(x, y) {
    withCanvasContext(function(canvas, ctx) {
        ctx.beginPath();
        ctx.arc(x-1, y-1, 2, 0, tau, false);
        ctx.closePath();
        ctx.strokeStyle = 'red';
        ctx.fillStyle = 'white';
        ctx.stroke();
        ctx.fill();
    });
}

function drawLine(start, end) {
    withCanvasContext(function(canvas, ctx) {
        ctx.beginPath();
        ctx.moveTo(start.x, start.y);
        ctx.lineTo(end.x, end.y);
        ctx.closePath();
        ctx.strokeStyle = 'red';
        ctx.stroke();
    });
}

function highlight(lines) {
    withCanvasContext(function(canvas, ctx) {
        ctx.beginPath();

        lines.forEach(function (line, index) {
            if (index == 0) {
                ctx.moveTo(line.start.x, line.start.y);
            }
            ctx.lineTo(line.end.x, line.end.y);
        });

        ctx.closePath();
        ctx.fillStyle = 'rgba(255, 0, 0, 0.5)';
        ctx.fill();
    });
}
