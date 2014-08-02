'use strict';

angular.module('whiteboard').service('$wix', function ($window, $log) {
  if ('Wix' in $window) {
    return $window.Wix;
  } else {
    return $log.error('Did you forget to include Wix.js?');
  }
});
