'use strict';
angular.module('whiteboard')
       .factory('server', function ($log, $http, $wix, $window, $q, $location) {


  var getInstance = function() {
    var instanceId;
    var url = $location.absUrl();
    var instanceRegexp = /.*instance=([\[\]a-zA-Z0-9\.\-_]*?)(&|$|#).*/g;
    var instance = instanceRegexp.exec(url);
    if (instance && instance[1]) {
      instanceId = instance[1]; //instanceId is actually the unparsed instance
    } else {
      $log.error('Getting Instance ID failed');
      return null;
    }
    return instanceId; //returns the unparsed instance
  };

  var compId = $wix.Utils.getOrigCompId() || $wix.Utils.getCompId() || 'test';
  var instance = getInstance() || 'xJWecKeRkkSLJdBAuoPZ26HsbfD3c7EwcAGA4SYV_ds.eyJpbnN0YW5jZUlkIjoiMTM4NWM3YTItNTA3OS1iM2FiLWY4NDQtMDE4OTNhMmVkNWU0Iiwic2lnbkRhdGUiOiIyMDE0LTA3LTMwVDE0OjUyOjM4LjU3Mi0wNTowMCIsInVpZCI6Ijc4MTdmOWYyLTcyMzEtNDkzZi05ODgyLTczN2IzZTQ4ZTE2MyIsInBlcm1pc3Npb25zIjoiT1dORVIiLCJpcEFuZFBvcnQiOiIxNjIuMjE3Ljc1LjM0LzQ1MDI0IiwidmVuZG9yUHJvZHVjdElkIjpudWxsLCJkZW1vTW9kZSI6ZmFsc2V9';


  // var getBoardThumbsURL = '/GetSettingsSettings/' + compId;
  var getSettingsURL = '/api/board/' + compId + '/settings';
  var saveSettingsURL = '/api/board/' + compId + '/settings';
  var getSubmittedUrl = '/api/board/' + compId + '/drawings?boardId=';
  var instanceHeader = {'X-Wix-Instance' : instance};
  var moderatorUrl = 'moderating.html?instance=' + instance + '&compId=' + compId;



  var getDrawings = function(boardId) {
    var deferred = $q.defer();
    $http({
      method: 'GET',
      url: getSubmittedUrl + boardId,
      headers: instanceHeader,
      timeout: 15000
    }).success(function (data, status) {
      if (status === 200) {
        deferred.resolve(data);
        console.log('Got drawings ', data);
      } else {
        console.log('The server is returning an incorrect status.');
        deferred.reject({});
      }
    }).error(function (message, status) {
      console.error(message, status);
      deferred.reject({});
    });

    return deferred.promise;
  };


  var getSettings = function() {
    var deferred = $q.defer();
    $http({
      method: 'GET',
      url: getSettingsURL,
      headers: instanceHeader,
      timeout: 15000
    }).success(function (data, status) {
      if (status === 200) {
        deferred.resolve(data);
        console.log('Got Settings ', data);
      } else {
        console.log('The server is returning an incorrect status.');
        deferred.reject({});
      }
    }).error(function (message, status) {
      console.error(message, status);
      deferred.reject({});
    });

    return deferred.promise;
  };



  var saveSettings = function(settings) {
    var deferred = $q.defer();
    $http({
      method: 'PUT',
      url: saveSettingsURL,
      headers: instanceHeader,
      dataType: 'json',
      timeout: 10000,
      data: JSON.stringify(settings)
    }).success(function (message, status) {
      if (status === 200) {
        console.debug('Saved settings', message);
        deferred.resolve();
      } else {
        console.log('The server is returning an incorrect status.');
        deferred.reject();
      }
    }).error(function (message, status) {
      console.log('Failed to save settings', message, status);
      deferred.reject();
    });
    return deferred.promise;
  };


  return {
    getDrawings: getDrawings,
    getSettings: getSettings,
    saveSettings: saveSettings,
    moderatorUrl: moderatorUrl
  };
});
