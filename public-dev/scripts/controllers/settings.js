'use strict';


angular.module('whiteboard')
  .controller('SettingsCtrl', function ($wix, server, $timeout, $log, $scope) {


  var ANSI = [
    { value: 'ANSI_A', name: 'A (Letter) - 8.5 × 11 in'},
    { value: 'ANSI_B', name: 'B (Tabloid) - 11 × 17 in'},
    { value: 'ANSI_C', name: 'C - 17 × 22 in'},
    { value: 'ANSI_D', name: 'D - 22 × 34 in'},
    { value: 'ANSI_E', name: 'E - 34 × 44 in'},
  ];

  var ISO = [
    {value: 'ISO_A4', name: 'A4 - 210 × 297 mm'},
    {value: 'ISO_A3', name: 'A3 - 148 × 210 mm'},
    {value: 'ISO_A2', name: 'A2 - 420 × 594 mm'},
    {value: 'ISO_A1', name: 'A1 - 594 × 841 mm'},
    {value: 'ISO_A0', name: 'A0 - 841 × 1189 mm'},
  ];



  var whiteColor = new Color(255, 255, 255, 1);
  var boardSettings = {};

  $wix.UI.onChange('*', function (value, key) {
    console.log('key: ', key);
    console.log('value: ', value);


    switch(key) {
    case 'canvasColor':
      whiteColor.fromRGBAColorString(value.rgba);
      boardSettings.backgroundColor = whiteColor.getColor();
      break;
    case 'paperStandard':
      $scope.paperStandard = value.value;
      $scope.$apply();
      break;
    case 'paperTypeANSI':
      boardSettings.paperType = value.value;
      break;
    case 'paperTypeISO':
      boardSettings.paperType = value.value;
      break;
    case 'borderWidth':
      boardSettings.design = {borderWidth: value};
      break;
    case 'boardMode':
      boardSettings.locked = value.value === 'locked';
      break;
    default:
      boardSettings[key] = value;
    }

    applyToWidget(boardSettings);
    saveSettingsDebounce();
  });



  var applyToWidget = function(settings) {
    $wix.Settings.triggerSettingsUpdatedEvent(settings,
                                              $wix.Utils.getOrigCompId());
  };


  function getIndexOfPaperType(paperType) {
    var i;
    for (i = 0; i < ANSI.length; i++) {
      if (ANSI.value === paperType || ISO.value === paperType) {
        return i;
      }
    }

    return -1;
  }


  function setAvailablePaperSizes(settings, isEmpty) {
    if(!isEmpty) {
      var indexOfCurrType = getIndexOfPaperType(settings.dimensions.paperType);
      ANSI = ANSI.slice(indexOfCurrType);
      ISO = ISO.slice(indexOfCurrType);
    }
  }


  var setSettings = function(data) {
    var settings = data.settings;
    setAvailablePaperSizes(settings, data.empty);
    boardSettings = settings;
    boardSettings.paperTypeANSI = settings.dimensions.paperType;
    boardSettings.paperTypeISO = settings.dimensions.paperType;
    boardSettings.paperType = settings.dimensions.paperType;
    boardSettings.boardMode = (settings.locked) ? 'locked' : 'drawable';

    var paperStandard = settings.dimensions.paperType.split('_')[0];
    boardSettings.paperStandard = paperStandard;


    boardSettings.borderWidth = settings.design.borderWidth;
    console.log('init1');
    $timeout(function() {
      $scope.paperStandard = paperStandard;
      $scope.paperTypes.ANSI = ANSI;
      $scope.paperTypes.ISO = ISO;
      $scope.$apply();
      console.log('init2');
      setTimeout(function() {$wix.UI.initialize(boardSettings);}, 5000);
    });
  };

  $scope.paperTypes = ANSI.concat(ISO);
  var getSettings = function() {
    server.getSettings()
      .then(function (response) {
        $log.info('got settings');
        var paperStandard = response.settings.dimensions.paperType.split('_')[0];
        setSettings(response);
      }, function(response) {
        $log.warn('rejected');
        setSettings(response);
      });
  };


  var saveSettings = function() {
    server.saveSettings(boardSettings).then(function() {});
  };

  var debounce = function(func, wait, immediate) {
    var timeout;
    return function() {
      $timeout.cancel(timeout);
      timeout = $timeout(function() {
        timeout = null;
        if (!immediate) {
          func.apply();
        }
      }, wait);
      if (immediate && !timeout) {
        func.apply();
      }
    };
  };

  var saveSettingsDebounce = debounce(saveSettings, 2000);

  console.log('init0');

  
     getSettings();
        
      
 


  // <a href='#' style='color: red; vertical-align: bottom; line-height: 30px; text-decoration: underline; margin-right: 6px;'>Clear Board</a> \


  var moderator;
  var moderatorWidth = 900;
  var moderatorHeight = 600;

  function openRequestedPopup() {
    var left = (screen.width/2)-(moderatorWidth/2);
    var top = (screen.height/2)-(moderatorHeight/2);
    var windowFeatures = 'toolbar=no, \
                          location=no, \
                          directories=no, \
                          status=no, \
                          menubar=no, \
                          scrollbars=no, \
                          resizable=no, \
                          copyhistory=no, \
                          width='+moderatorWidth+', \
                          height='+moderatorHeight+', \
                          top='+top+', \
                          left='+left;
    moderator = window.open(server.moderatorUrl, boardSettings.boardName, windowFeatures);
  }

  jQuery('#edit-drawings').on('click', function(){
    openRequestedPopup();
  });

});




