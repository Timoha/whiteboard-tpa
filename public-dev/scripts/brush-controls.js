(function ($) {
  'use strict';

  var colors = [
    {red: '255', green: '147', blue: '30',  name: "orange"},
    {red: '237', green: '28', blue: '36',   name: "red"},
    {red: '255', green: '123', blue: '172', name: "pink" },
    {red: '41', green: '171', blue: '226',  name: "blue"},
    {red: '46', green: '49', blue: '146',   name: "dark blue"},
    {red: '102', green: '45', blue: '145',  name: "purple"},
    {red: '252', green: '238', blue: '33',  name: "yellow"},
    {red: '140', green: '198', blue: '63',  name: "green"},
    {red: '0', green: '104', blue: '55',    name: "dark green"},
    {red: '255', green: '255', blue: '255', name: "white"},
    {red: '117', green: '76', blue: '36',   name: "brown"},
    {red: '0', green: '0', blue: '0',       name: "black"}
  ];


  var currBrush;

  $.constructColor = function (color) {
    color.rgb = [color.red, color.green, color.blue].join(',');
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
    $('#opacity, #radius').rangeslider({

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
