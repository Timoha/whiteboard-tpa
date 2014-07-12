$(document).ready(function() {

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


  $('#colors').initColors('#brush');

});
