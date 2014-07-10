'use strict';

angular.module('happyBoard', [])
  .controller('SettingsCtrl', ['$scope', '$wix', function ($scope, $wix) {

//     var instance = api.getInstance();
//     $scope.compId = $wix.Utils.getOrigCompId() || $wix.Utils.getCompId();
//     $scope.instance = instance;
    $wix.UI.initialize();
    $wix.UI.onChange('*', function (value, key) {
//       console.log("key: ", key);
//       console.log("value: ", value);

//       if (key === 'widgetCorners' || key === 'buttonCorners' || key === 'borderWidth') { // if the settings changed is a button etc
//         $scope.settings[key] = value.value;
//       } else {
//         $scope.settings[key] = value;
//       }
      // $scope.settings[key] = value;
//   	  $wix.Settings.triggerSettingsUpdatedEvent($scope.settings,
//   		  'test');
    });
    $scope.settings = {
      paperStandard: 'ANSI'
    };

    Wix.UI.onChange('paperStandard', function(value, key){
      $scope.settings.paperStandard = value.value;
      $scope.$apply();
    });

    Wix.UI.onChange('canvasSize', function(value, key){
      console.log("key: ", key);
      console.log("value: ", value);
    });

    Wix.UI.onChange('canvasColor', function(value, key){
      console.log("key: ", key);
      console.log("value: ", value);
    });
}]);
