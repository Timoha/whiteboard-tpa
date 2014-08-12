'use strict';

function Brush(size, color) {
  this.size = size;
  this.color = color;

  this.setSize = function (size) {
    this.size = size;
  };
}


$(document).ready(function () {


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


    var storedState = localStorage.getItem('elm-whiteboard-drawingInfo');
    var drawingInfo = storedState ? JSON.parse(storedState) : null;


    function enableEditingMode() {
      editor.ports.actionPort.send('Draw');
      $('.viewing').hide();
      $('.editing').show();
      $('#color-tool').parent().addClass('tab-open');
      $('.active').removeClass('active');
      $('#color-tool').addClass('active');
      $('#colors').initColorPanel('#brush', defaultBrush);
    }

    function enableViewingMode() {
      editor.ports.actionPort.send('View');
      $('.editing').hide();
      $('.viewing').show();
      $('.active').removeClass('active');
      $('#drag-tool').addClass('active');
      storedState = localStorage.getItem('elm-whiteboard-drawingInfo');
      drawingInfo = storedState ? JSON.parse(storedState) : null;
    }



    var editor = Elm.embed(
        Elm.Editor,
        document.getElementById('canvas'),
        {
          brushPort: defaultBrush,
          actionPort: 'NoOp',
          userInfoPort: drawingInfo,
          canvasSizePort: dimensions,
          boardInfoPort: boardInfo,
          submittedDrawingsPort: null,
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
    editor.ports.submittedDrawingsPort.send(submitted);
    enableViewingMode();


    // make following tools active on click
    $('#color-tool, #eraser-tool, #drag-tool, #start-tool').on('click', function () {
      $('.active').toggleClass('active');
      $(this).toggleClass('active');
    });


    function toggleTab() {
      var element = $(this);
      $('.tab-open').not(element.parent()).removeClass('tab-open');
      element.parent().toggleClass('tab-open');
    }

    // open/close tab for following tools
    $('#color-tool, #drag-tool, #start-tool').on('mousedown', toggleTab);

    if(drawingInfo !== null) {
      $('#start-tool').off('mousedown', toggleTab);
    }


    // close tab if clicked anywhere outside it
    $(document).on('mousedown touchstart', function (e) {
      var activeTool = $('.tab-open');
      if (!activeTool.is(e.target)
          && activeTool.has(e.target).length === 0) {
        editor.ports.canvasOut.unsubscribe(updateMinimap);
        activeTool.removeClass('tab-open');
      }
    });


    $('#start-tool').on('click', function () {
      var startTool = $(this);
      if(drawingInfo !== null) {
        $.ajax({
          type: 'POST',
          url:  '/api/board/' + boardSettings.boardId + '/resume_drawing',
          dataType: 'json',
          data: JSON.stringify(drawingInfo),
        }).done(function (data, status) {
          console.log('resumed drawing', data);
          enableEditingMode();
        }).fail(function (data, status, message) {
          console.error('cannot resume drawing', message, status, data);
          localStorage.removeItem('elm-whiteboard-drawingInfo');
          startTool.parent().addClass('tab-open');
          startTool.on('mousedown', toggleTab);
        });
      }
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



    function isEmail(email) {
      var regex = /^([a-zA-Z0-9_.+-])+\@(([a-zA-Z0-9-])+\.)+([a-zA-Z0-9]{2,4})+$/;
      return regex.test(email);
    }


    $( "input[name='first-name'], input[name='last-name']" ).on('keyup keypress blur change', function () {
      var elem = $(this);
      if(elem.val()) {
        elem.removeClass('invalid');
      } else {
        elem.addClass('invalid');
      }
    });


    $( "input[name='email']" ).on('keyup keypress blur change', function () {
      var elem = $(this);
      if(isEmail(elem.val())) {
        elem.removeClass('invalid');
      } else {
        elem.addClass('invalid');
      }
    });


    // refactor this
    $('#start-drawing').on('click', function () {

      if(!$( "input[name='first-name']" ).val()) {
        $( "input[name='first-name']" ).addClass('invalid');
      }

      if(!$( "input[name='last-name']" ).val()) {
        $( "input[name='last-name']" ).addClass('invalid');
      }

      if(!isEmail($( "input[name='email']" ).val())) {
        $( "input[name='email']" ).addClass('invalid');
      }

      var isValidInfo = !($( "input[name='email'], input[name='first-name'], input[name='last-name']" ).is('.invalid'));
      if (isValidInfo) {
        var user = {
          firstName: $( "input[name='first-name']" ).val(),
          lastName: $( "input[name='last-name']" ).val(),
          email: $( "input[name='email']" ).val()
        };
        $.ajax({
          type: 'POST',
          url:  '/api/board/' + boardSettings.boardId + '/new_drawing',
          dataType: 'json',
          data: JSON.stringify(user),
          success: function (data) {
            var drawingState = data;
            delete drawingState.strokes;
            console.log('got drawing info', data);
            $('#start-tool').off('mousedown', toggleTab);
            localStorage.setItem('elm-whiteboard-drawingInfo', JSON.stringify(drawingState));
            editor.ports.userInfoPort.send(data);
            enableEditingMode();
          }
        });
      }
    });


    function submitDrawing(drawing) {
      console.log("submit", drawing);
      $.ajax({
        type: 'PUT',
        url:  '/api/board/' + boardSettings.boardId  + '/submit_drawing',
        dataType: 'json',
        data: JSON.stringify(drawing)
      }).done(function (data, status) {
        console.log('submitted drawing', data);
        enableViewingMode();
        editor.ports.drawingOut.unsubscribe(submitDrawing);
      }).fail(function (data, status, message) {
        console.error('cannot submit drawing', message, status, data);
        editor.ports.drawingOut.unsubscribe(submitDrawing);
      });
    }

    $('#done-drawing').on('click', function () {
      editor.ports.actionPort.send('View');
      editor.ports.drawingOut.subscribe(submitDrawing);
    });

  }


  getSettings().done(function (data, status) {
    console.log('got settings', data);
    setupBoard(data);
  }).fail(function (data, status, message) {
    console.error('cannot get settings', message, status, data);
  });


});
