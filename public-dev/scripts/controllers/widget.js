'use strict';

angular.module('whiteboard')
  .controller('WidgetCtrl', function ($scope, $wix) {

    $scope.settings = {};


    /**
     * When the site owner updates the settings, this function allows the
     * widget to implement these changes immediately.
     */
    $wix.addEventListener($wix.Events.SETTINGS_UPDATED, function(message) {
      $scope.settings = message;
      $scope.$apply();
    });

  });
