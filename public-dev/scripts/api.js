'use strict';


var getInstance = function() {
  var instanceId;
  var url = document.URL;
  var instanceRegexp = /.*instance=([\[\]a-zA-Z0-9\.\-_]*?)(&|$|#).*/g;
  var instance = instanceRegexp.exec(url);
  if (instance && instance[1]) {
    instanceId = instance[1]; //instanceId is actually the unparsed instance
  } else {
    console.error('Getting Instance ID failed');
    return null;
  }
  return instanceId; //returns the unparsed instance
};

var compId = Wix.Utils.getOrigCompId() || Wix.Utils.getCompId();
var instance = getInstance();

var getSettingsURL = '/api/board/' + compId;
var instanceHeader = {'X-Wix-Instance' : instance};


function getSettings() {
  return $.ajax({
    type: 'GET',
    url: getSettingsURL,
    timeout: 15000,
    headers: instanceHeader,
    dataType: 'json text'
  });
}


