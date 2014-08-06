'use strict';


function Brush(size, color) {
  this.size = size;
  this.color = color;

  this.setSize = function (size) {
    this.size = size;
  };
}

$(document).ready(function() {



  var boardSettings = {}

  var defaultCanvasColor = new Color(255, 255, 255, 1);
  var defaultBrushColor = new Color(255, 147, 30, 1)
  var defaultBrush = new Brush(8, defaultBrushColor);


  getSettings().done(function (data, status) {
    var data = JSON.parse(data);
    console.log('got settings', data);
    setupBoard(data);
  }).fail(function (data, status, message) {
    console.error('cannot get settings', message, status, data);
  });





  function setupBoard(data) {

    var settings = data.settings;
    var drawings = data.submitted;

    $('#board-title').text(settings.boardName);

    var dimensions = settings.dimensions;
    delete dimensions.paperType;


    var boardInfo = {
      instance: instance,
      componentId: compId,
      boardId: settings.boardId
    };


    var editor = Elm.embed(
      Elm.Editor,
      document.getElementById('canvas'),
      { brushPort: defaultBrush
      , actionPort: 'Moderate'
      , userInfoPort: null
      , canvasSizePort: dimensions
      , boardInfoPort: boardInfo
      , submittedDrawingsPort: drawings
      });

    editor.ports.actionPort.send("Moderate");
    var minimap = Elm.embed(
      Elm.Minimap,
      document.getElementById('minimap'),
      {canvasPort: null});


    $('#loading').hide();
    $('.tool-panel').show();


    // make following tools active on click
    $('#eraser-tool, #drag-tool').on('click', function () {
      $('.active').toggleClass('active');
      $(this).toggleClass('active');
    });



    $('#eraser-tool').on('click', function () {
      editor.ports.actionPort.send("Moderate");
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


    function updateMinimap(canvas) {
      minimap.ports.canvasPort.send(canvas);
    }


    editor.ports.erasedDrawingIdsOut.subscribe(parent.setDeleteDrawingsIds);



    $('#drag-tool').on('mousedown', function () {
      editor.ports.canvasOut.subscribe(updateMinimap);
    });

    // open/close tab for following tools
    $('#drag-tool').on('mousedown', function () {
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
  }

});
