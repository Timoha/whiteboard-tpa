function Brush(size, r, g, b, a) {
  this.size = size;
  this.red = r;
  this.green = g;
  this.blue = b;
  this.alpha = a;

  this.setSize = function (size) {
    this.size = size;
  };

  this.setR = function (r) {
    this.red = r;
  };

  this.setG = function (g) {
    this.green = g;
  };

  this.setB = function (b) {
    this.blue = b;
  };

  this.setA = function (a) {
    this.alpha = a;
  };

  this.toRGBColorString = function () {
    var color = [this.red, this.green, this.blue];
    return 'rgb(' + color.join(',')  +  ')';
  }

  this.fromRGBColorString = function (rgb) {
    var color = rgb.slice(4, rgb.length - 1).split(',');
    this.red = parseInt(color[0], 10);
    this.green = parseInt(color[1], 10);
    this.blue = parseInt(color[2], 10);
  }
}

$(document).ready(function() {

  var defaultBrush = new Brush(8, 255, 147, 30, 1);

  var canvas = Elm.embed(Elm.Canvas, document.getElementById('canvas'), {newBrush: defaultBrush, undoAction: []});


  // open/close tab for following tools
  $('#color-tool, #drag-tool').mousedown(function () {
    var element = $(this);
    $('.tab-open').not(element.parent()).removeClass('tab-open');
    element.parent().toggleClass('tab-open');
  });

  // close tab if clicked anywhere outside it
  $(document).on('mousedown touchstart', function (e) {
    var activeTool = $('.tab-open');

    if (!activeTool.is(e.target)
        && activeTool.has(e.target).length === 0) {
      activeTool.removeClass('tab-open');
    }
  });

  // make following tools active on click
  $('#color-tool, #eraser-tool, #drag-tool, #help-tool').click(function () {
    $('.active').toggleClass('active');
    $(this).toggleClass('active');
  });


  $('#colors').initColorPanel('#brush', defaultBrush);

  $('#brush').on('brush_change', function (event, data) {
    canvas.ports.newBrush.send(data);
  });

  $('#undo-tool').click(function () {
    canvas.ports.undoAction.send([]);
  });

});
