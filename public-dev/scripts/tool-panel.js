'use strict';

function Brush(size, color) {
  this.size = size;
  this.color = color;

  this.setSize = function (size) {
    this.size = size;
  };
}


$(document).ready(function () {

  $(".editing").hide();

  var boardSettings = {};
  var defaultBrushColor = new Color(255, 147, 30, 1);
  var defaultBrush = new Brush(8, defaultBrushColor);

  function setSettings(settings) {

    boardSettings = settings;
    if (settings.locked) {
      $('#start-tool').parent().hide();
    } else {
      $('#start-tool').parent().show();
    }

    var borderWidth = settings.design.borderWidth;
    $('.main-body').css({
      'border-width': borderWidth + 'px'
    });

  }


  Wix.addEventListener(Wix.Events.SETTINGS_UPDATED, function (message) {
    console.log('settings from wix', message);
    setSettings(message);
  });


  function setupBoard(data) {

    var settings = data.settings;
    var submitted = data.submitted;

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
      {
        brushPort: defaultBrush,
        actionPort: 'View',
        userInfoPort: null,
        canvasSizePort: dimensions,
        boardInfoPort: boardInfo,
        submittedDrawingsPort: submitted
      }
    );


    var minimap = Elm.embed(
      Elm.Minimap,
      document.getElementById('minimap'),
      {canvasPort: null}
    );


    setSettings(settings);
    $('#loading').hide();
    $('.tool-panel').show();


    // make following tools active on click
    $('#color-tool, #eraser-tool, #drag-tool, #start-tool').on('click', function () {
      $('.active').toggleClass('active');
      $(this).toggleClass('active');
    });


    $('#color-tool').on('click', function () {
      editor.ports.actionPort.send("Draw");
    });

    $('#eraser-tool').on('click', function () {
      editor.ports.actionPort.send("Erase");
    });


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



    function updateMinimap(canvas) {
      minimap.ports.canvasPort.send(canvas);
    }


    $('#drag-tool').on('mousedown', function () {
      editor.ports.canvasOut.subscribe(updateMinimap);
    });

    // open/close tab for following tools
    $('#color-tool, #drag-tool, #start-tool').on('mousedown', function () {
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


    var drawingInfo = {};

    $('#start-drawing').on('click', function () {
      var user = {
        firstName: $( "input[name='first-name']" ).val(),
        lastName: $( "input[name='last-name']" ).val(),
        email: $( "input[name='email']" ).val()
      };

      $.ajax({
        type: 'POST',
        url:  'http://localhost:9160/api/board/' + boardSettings.boardId + '/drawing',
        dataType: 'json',
        data: JSON.stringify(user),
        success: function( data ) {
          drawingInfo = JSON.parse(data);
          editor.ports.userInfoPort.send(drawingInfo);
          editor.ports.actionPort.send('Draw');
          $('.viewing').hide();
          $('.editing').show();
          $('#colors').initColorPanel('#brush', defaultBrush);
        }
      });
    });

    function submitDrawing(drawing) {
      console.log("submit", drawing);
      $.ajax({
        type: 'POST',
        url:  'http://localhost:9160/api/drawing/' + drawingInfo.drawingId + '/submit',
        dataType: 'json',
        data: JSON.stringify(drawing.drawing),
        success: function( data ) {
          console.log('so?', data);
          $(".editing").hide();
          $(".viewing").show();
          editor.ports.canvasOut.unsubscribe(submitDrawing);
        }
      });
    }

    $('#done-drawing').on('click', function () {
      editor.ports.actionPort.send("View");
      editor.ports.canvasOut.subscribe(submitDrawing);
    });

  }


  getSettings().done(function (data, status) {
    data = JSON.parse(data);
    console.log('got settings', data);
    setupBoard(data);
  }).fail(function (data, status, message) {
    console.error('cannot get settings', message, status, data);
  });

});
