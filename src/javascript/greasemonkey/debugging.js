// Copyright (c) 2009, P.G. Schaaf
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html

var debugLevel = 0;

// ===========================================================
function DebugInfoHeader() {
  this.text = '';
}

DebugInfoHeader.prototype.length = function() {
  return this.text.length;
};

DebugInfoHeader.prototype.add = function(message) {
  this.text += message + '<br/>';
};

DebugInfoHeader.prototype.addValue = function(name, value) {
  this.add(name + ' == ' + value);
};

DebugInfoHeader.prototype.addProperties = function(name, object) {
  if (object == null) {
    this.addValue(name, object);
  }
  else {
    // not sure how to do this correctly in firefox
    for(prop in object) {
      this.addValue(name + '[' + prop + ']', object[prop]);
    }
  }
};

DebugInfoHeader.prototype.show = function() {
  if (debugLevel > 1 || (debugLevel == 1 && debugInfo.length() > 0)) {
    var debugElem = document.createElement('h1');
    var html = '<div id="PGS_debug" style="'
      + 'margin: 0 auto 0 auto;     border-bottom: 1px solid #000000;'
      + 'margin-bottom: 5px;        font-size: large;'
      + 'background-color: yellow;  color: black;'
      + 'text-align: left;'
      + '">'
      + '<u>DEBUG INFO</u> (debugLevel = ' + debugLevel
      + ', browser = ' + browser + ')'
      + '<p style="margin: 2px 0 1px 0; font-size: small;">'
      + '<i>This is Paul Schaaf\'s GW Greasemonkey Script</i><br/>';

    if (debugLevel > 2) {
      // Show browser properties
      this.addValue('navigator.appName',     navigator.appName);
      this.addValue('navigator.appCodeName', navigator.appCodeName);
      this.addValue('navigator.appVersion',  navigator.appVersion);
      this.addValue('navigator.product',     navigator.product);
      this.addValue('navigator.vendor',      navigator.vendor);
      if (debugLevel > 3) {
        this.addProperties('navigator',      navigator);
      }
    }

    debugElem.innerHTML = html
      + this.text
      + '</p>'
      + '</div>';

    if (browser == 'msie') {
      document.insertBefore(debugElem, document.firstChild);
    }
    else {
      document.body.insertBefore(debugElem, document.body.firstChild);
    }
  };
};

var debugInfo = new DebugInfoHeader();

window.setTimeout(function() { debugInfo.show(); });
