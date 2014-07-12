(function (document, $) {
  'use strict';

  var colors = [
    {rgb: '255, 147, 30',  name: "orange"},
    {rgb: '237, 28, 36',   name: "red"},
    {rgb: '255, 123, 172', name: "pink" },
    {rgb: '41, 171, 226',  name: "blue"},
    {rgb: '46, 49, 146',   name: "dark blue"},
    {rgb: '102, 45, 145',  name: "purple"},
    {rgb: '252, 238, 33',  name: "yellow"},
    {rgb: '140, 198, 63',  name: "green"},
    {rgb: '0, 104, 55',    name: "dark green"},
    {rgb: '255, 255, 255', name: "white"},
    {rgb: '117, 76, 36',   name: "brown"},
    {rgb: '0, 0, 0',       name: "black"}
  ];




  $.constructColor = function (color) {
    var rgba = "rgb(" + color.rgb + ")";
    return $( "<option>", {
      value: rgba,
      text: color.name,
      class: "color"
    });
  };


  $.initColorPicker = function ($colorPicker, $brush) {

    $brush.css('background-color', $colorPicker.val());
    $colorPicker.simplecolorpicker({theme: 'cf'}).on('change', function() {
      $brush.css('background-color', $colorPicker.val());
    });
  };



  $.initSliders = function ($brush) {
    var value = 0;
    var pxValue = '';
    $('#radius').on('change',  function (e) {
      value = Number(e.target.value);
      pxValue = value + "px";
      $brush.css({'width': pxValue, 'height': pxValue, 'border-radius': pxValue});
      pxValue = (value + 6) + "px";
      $brush.parent().css({'width': pxValue, 'height': pxValue, 'border-radius': pxValue})

    });

    var opacity = 1;
    $('#opacity').on('change',  function (e) {
      opacity = Number(e.target.value) / 100;
      $brush.css({'opacity': opacity});
    });

  };


  $.fn.initColors = function (brushSelector) {
    var $colorPicker = this;
    var $brush = $(brushSelector);

    $colorPicker.each(function () {
      colors.map(function (color) {
        $.constructColor(color).appendTo($colorPicker);
      });
    });


    $.initColorPicker($colorPicker, $brush);

    $.initSliders($brush);

    return;
  };

}(document, jQuery ));