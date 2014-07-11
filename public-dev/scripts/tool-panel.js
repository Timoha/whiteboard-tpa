$(document).ready(function() {

  $('#color-tool, #drag-tool').mousedown(function () {
    var element = $(this);
    $('.tab-open').not(element.parent()).removeClass('tab-open');
    element.parent().toggleClass('tab-open');
  });

  $(document).mousedown(function (e) {
    var activeTool = $('.tab-open');

    if (!activeTool.is(e.target)
        && activeTool.has(e.target).length === 0) {
      activeTool.removeClass('tab-open');
    }
  });

  $('#color-tool, #eraser-tool, #drag-tool, #help-tool').click(function () {
    $('.active').toggleClass('active');
    $(this).toggleClass('active');
  });


});