// Copyright (c) 2009, P.G. Schaaf
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html

function DebugInfoHeader(debugLevel) {
  if (navigator.appName.match(/Microsoft/)) this.browser = 'msie';
  else if (navigator.vendor.match(/Google/)) this.browser = 'chrome';
  else this.browser = 'firefox';

  var self = this;
  this.debugLevel = debugLevel;
  this.text = '';
  document.PGS_debugInfo = this;

  if (this.debugLevel > 0) window.setTimeout(function () {
    self.show();
  });

  this.add = function (message) {
    this.text += message + '<br/>';
    return this;
  };

  this.addValue = function (name, value) {
    return this.add(name + ' == ' + value);
  };

  this.addValuesOf = function (objName, obj, propNames) {
    propNames.forEach(function (propName) {
      self.addValue(objName + '.' + propName, obj[propName]);
    });
    return this;
  };

  this.insert = function (node) {
    document.body.insertBefore(node, document.body.firstChild);
  };

  this.show = function () {
    if (this.debugLevel > 1 || (this.debugLevel === 1 && this.text.length > 0)) {
      self.debugElem = document.createElement('h1');
      var html = '<div id="PGS_debug" style="'
          + 'border-bottom: 1px solid #000000;'
          + 'margin: 0 auto 5px;font-size: large;'
          + 'background-color: yellow;  color: black;'
          + 'text-align: left;'
          + '">'
          + '<u>DEBUG INFO</u> (debugLevel = ' + this.debugLevel + ', browser = ' + this.browser + ')'
          + '<p style="margin: 2px 0 1px 0; font-size: small;">'
          + '<i>This is Paul Schaaf\'s GW Greasemonkey Script</i><br/>';

      if (this.debugLevel > 2) {
        this.addValuesOf(
            'navigator',
            navigator,
            ['appName', 'appCodeName', 'appVersion', 'product', 'vendor']
        );
      }

      self.debugElem.innerHTML = html + this.text + '</p>' + '</div>';

      self.insert(self.debugElem)
    }
  };
}

