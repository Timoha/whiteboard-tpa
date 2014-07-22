'use strict';

function Brush(size, r, g, b, a) {
  this.size = size;
  this.color = {};
  this.color.red = r;
  this.color.green = g;
  this.color.blue = b;
  this.color.alpha = a;

  this.setSize = function (size) {
    this.size = size;
  };

  this.setR = function (r) {
    this.color.red = r;
  };

  this.setG = function (g) {
    this.color.green = g;
  };

  this.setB = function (b) {
    this.color.blue = b;
  };

  this.setA = function (a) {
    this.color.alpha = a;
  };

  this.toRGBColorString = function () {
    var color = [this.color.red, this.color.green, this.color.blue];
    return 'rgb(' + color.join(',')  +  ')';
  }

  this.fromRGBColorString = function (rgb) {
    var color = rgb.slice(4, rgb.length - 1).split(',');
    this.color.red = parseInt(color[0], 10);
    this.color.green = parseInt(color[1], 10);
    this.color.blue = parseInt(color[2], 10);
  }
}

$(document).ready(function() {

  var defaultBrush = new Brush(8, 255, 147, 30, 1);

  var editor = Elm.embed(Elm.Editor, document.getElementById('canvas'),
                         {brushPort: defaultBrush,
                          actionPort: "Draw"}
                        );

  var minimap = Elm.embed(Elm.Minimap, document.getElementById('minimap'),
                          {canvasPort: null});

  function updateMinimap(canvas) {
    minimap.ports.canvasPort.send(canvas);
  }


  $('#drag-tool').on('mousedown', function () {
    editor.ports.canvasOut.subscribe(updateMinimap);
  });

  // open/close tab for following tools
  $('#color-tool, #drag-tool').on('mousedown', function () {
    var element = $(this);
    $('.tab-open').not(element.parent()).removeClass('tab-open');
    element.parent().toggleClass('tab-open');
  });

  // close tab if clicked anywhere outside it
  $(document).on('mousedown touchstart', function (e) {
    var activeTool = $('.tab-open');
    if (!activeTool.is(e.target)
        && activeTool.has(e.target).length === 0) {
      editor.ports.canvasOut.unsubscribe(updateMinimap);
      activeTool.removeClass('tab-open');

    }
  });

  // make following tools active on click
  $('#color-tool, #eraser-tool, #drag-tool, #help-tool').on('click', function () {
    $('.active').toggleClass('active');
    $(this).toggleClass('active');
  });


  $('#color-tool').on('click', function () {
    editor.ports.actionPort.send("Draw");
  });

  $('#eraser-tool').on('click', function () {
    editor.ports.actionPort.send("Erase");
  });


  $('#colors').initColorPanel('#brush', defaultBrush);

  $('#brush').on('brush_change', function (event, data) {
    editor.ports.brushPort.send(data);
  });

  $('#undo-tool').on('click', function () {
    editor.ports.actionPort.send("Undo");
  });

  $('#drag-tool').on('click', function () {
    editor.ports.actionPort.send("View");
  });

  $('#zoomIn-tool').on('click', function () {
    editor.ports.actionPort.send("ZoomIn");
  });

  $('#zoomOut-tool').on('click', function () {
    editor.ports.actionPort.send("ZoomOut");
  });

});
