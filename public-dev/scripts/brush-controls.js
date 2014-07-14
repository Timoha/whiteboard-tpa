(function ($) {
  'use strict';

  var colors = [
    {r: '255', g: '147', b: '30',  name: "orange"},
    {r: '237', g: '28', b: '36',   name: "red"},
    {r: '255', g: '123', b: '172', name: "pink" },
    {r: '41', g: '171', b: '226',  name: "blue"},
    {r: '46', g: '49', b: '146',   name: "dark blue"},
    {r: '102', g: '45', b: '145',  name: "purple"},
    {r: '252', g: '238', b: '33',  name: "yellow"},
    {r: '140', g: '198', b: '63',  name: "green"},
    {r: '0', g: '104', b: '55',    name: "dark green"},
    {r: '255', g: '255', b: '255', name: "white"},
    {r: '117', g: '76', b: '36',   name: "brown"},
    {r: '0', g: '0', b: '0',       name: "black"}
  ];


  var currBrush;

  $.constructColor = function (color) {
    color.rgb = [color.r, color.g, color.b].join(',');
    var rgb = "rgb(" + color.rgb + ")";
    return $( "<option>", {
      value: rgb,
      text: color.name,
    });
  };


  $.initColorPicker = function ($colorPicker, $brush) {
    var newColor = $colorPicker.val();
    $brush.css('background-color', newColor);
    currBrush.fromRGBColorString(newColor);

    $colorPicker.simplecolorpicker({theme: 'cf'}).on('change', function() {
      newColor = $colorPicker.val();
      $brush.css('background-color', newColor);
      currBrush.fromRGBColorString(newColor);
      $brush.trigger('brush_change', [currBrush]);
    });
  };



  $.initSliders = function ($brush) {
    $('[data-rangeslider]').rangeslider({

            // Deactivate the feature detection
      polyfill: false,
      rangeClass: 'rangeslider',
      fillClass: 'rangeslider-fill',
      handleClass: 'rangeslider-handle',
      onSlideEnd: function(position, value) {
        $brush.trigger('brush_change', [currBrush]);
      }
    });


    var value = 0;
    var pxValue = '';
    $('#radius').on('change',  function (e) {
      value = Number(e.target.value);
      pxValue = value + 'px';
      $brush.css({'width': pxValue, 'height': pxValue, 'border-radius': pxValue});
      pxValue = (value + 6) + 'px';
      $brush.parent().css({'width': pxValue, 'height': pxValue, 'border-radius': pxValue})
      currBrush.setSize(value);
    });

    var opacity = 1;
    $('#opacity').on('change',  function (e) {
      opacity = Number(e.target.value) / 100;
      $brush.css({'opacity': opacity});
      currBrush.setA(opacity);
    });

  };


  $.fn.initColorPanel = function (brushSelector, defaultBrush) {
    var $colorPicker = this;
    var $brush = $(brushSelector);
    currBrush = defaultBrush;

    $colorPicker.each(function () {
      colors.map(function (color) {
        $.constructColor(color).appendTo($colorPicker);
      });
    });


    $.initColorPicker($colorPicker, $brush);

    $.initSliders($brush);

    return;
  };

}( jQuery ));
