function Brush(size, r, g, b, a) {
  this.size = size;
  this.r = r;
  this.g = g;
  this.b = b;
  this.a = a;

  this.setSize = function (size) {
    this.size = size;
  };

  this.setR = function (r) {
    this.r = r;
  };

  this.setG = function (g) {
    this.g = g;
  };

  this.setB = function (b) {
    this.b = b;
  };

  this.setA = function (a) {
    this.a = a;
  };

  this.toRGBColorString = function () {
    var color = [this.r, this.g, this.b];
    return 'rgb(' + color.join(',')  +  ')';
  }

  this.fromRGBColorString = function (rgb) {
    var color = rgb.slice(4, rgb.length - 1).split(',');
    this.r = parseInt(color[0], 10);
    this.g = parseInt(color[1], 10);
    this.b = parseInt(color[2], 10);
  }
}

$(document).ready(function() {





  var defaultBrush = new Brush(8, 255, 147, 30, 1);

  var canvas = Elm.embed(Elm.Canvas, document.getElementById('canvas'), {newBrush: defaultBrush});


  // open/close tab for following tools
  $('#color-tool, #drag-tool').mousedown(function () {
    var element = $(this);
    $('.tab-open').not(element.parent()).removeClass('tab-open');
    element.parent().toggleClass('tab-open');
  });

  // close tab if clicked anywhere outside it
  $(document).mousedown(function (e) {
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
  })

});
