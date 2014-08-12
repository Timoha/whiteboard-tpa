'use strict';


$(document).ready(function() {


  getSettings().done(function (data, status) {
    console.log('got settings', data);
    setupBoard(data);
  }).fail(function (data, status, message) {
    console.error('cannot get settings', message, status, data);
  });

  var currBoardId = null;

  function setupBoard(data) {

    var settings = data.settings;
    var drawings = data.submitted;

    currBoardId = settings.boardId;

    $('#board-title').text(settings.boardName);

    var dimensions = settings.dimensions;
    delete dimensions.paperType;


    var editor = Elm.embed(
      Elm.Moderator,
      document.getElementById('canvas'),
      { actionPort: 'View'
      , canvasSizePort: dimensions
      , submittedDrawingsPort: drawings
      });

    var minimap = Elm.embed(
      Elm.Minimap,
      document.getElementById('minimap'),
      {canvasPort: null});


    $('#loading').hide();
    $('.tool-panel').show();

    $('#drag-tool').addClass('active');


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

    var drawingsIdsToDelete = [];

    function setDeleteDrawingsIds(ids) {
      drawingsIdsToDelete = ids;
    }
    editor.ports.erasedDrawingIdsOut.subscribe(setDeleteDrawingsIds);


    $('#save-progress').on('click', function () {
      deleteDrawings(drawingsIdsToDelete, currBoardId).done(function (data, status) {
        console.log('removed drawings', data);
        window.close();
      }).fail(function (data, status, message) {
        console.error('cannot remove drawings', message, status, data);
      });
    });


    $('#download-board').on('click', function () {
        $.fileDownload('/api/board/'+currBoardId+'/download');
    });


    editor.ports.actionPort.send('View');
  }

});
