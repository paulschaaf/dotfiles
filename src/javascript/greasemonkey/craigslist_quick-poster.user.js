// ==UserScript==
// @name           Craigslist Quick-Poster
// @namespace      tag:paul.schaaf@gmail.com,2010-01-01
// @description    Automate postings to Craigslist from URL parameters
// @match          https://post.craigslist.org/*
// ==/UserScript==

// Craigslist Quick-Poster user script
// version 0.5 BETA!
// 2009/08/07
// Copyright (c) 2010, P.G. Schaaf
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// -------------------------------------------------------------------------
//
// This is a Greasemonkey user script.
//
// -------------------------------------------------------------------------

function PGS_mod_namespace () {
  var debugLevel = 1;

  // var browser;
  // if (navigator.appName.match(/Microsoft/)) {
  //   browser = 'msie';
  // }
  // else if (navigator.vendor.match(/Google/)) {
  //   browser = 'chrome';
  // }
  // else {
  //   browser = 'firefox';
  // }

  // Array.prototype.swap = function(x,y) {
  //   var tmp = this[x];
  //   this[x] = this[y];
  //   this[y] = tmp;
  //   return this;
  // };

  // ===========================================================
  function DebugInfoHeader() {
    this.debugText = ''; //url == ' + location.href + '<br/>';
  }

  DebugInfoHeader.prototype.add = function(message) {
    this.debugText += message + '<br/>';
  };

  DebugInfoHeader.prototype.addValue = function(name, value) {
    this.add(name + ' == ' + value);
  };

  DebugInfoHeader.prototype.show = function() {
    var debugElem = document.createElement('h1');
    debugElem.innerHTML = '<div id="PGS_debug" style="'
      + 'margin: 0 auto 0 auto;     border-bottom: 1px solid #000000;'
      + 'margin-bottom: 5px;        font-size: large;'
      + 'background-color: yellow;  color: black;">'
      + '<u>DEBUG INFO</u>'
      + '<p style="margin: 2px 0 1px 0; font-size: medium;">'
      + 'debugLevel == ' + eval('debugLevel') + '<br/>'
      + '<br/>last updated: '
      + 'Fri 2010-08-13 -  3:27 PM'
      + this.debugText
      + '</p>'
      + '</div>';
    document.body.insertBefore(debugElem, document.body.firstChild);
  };

  DebugInfoHeader.prototype.length = function() {
    return this.debugText.length;
  };

  var debugInfo = new DebugInfoHeader();

  window.setTimeout(
    function() {
      if (debugInfo.length() > 0) {
        debugInfo.show();
      }
    }
  );

  // ===========================================================
  var thisForm   = document.forms[0];

  function collectUserParams() {
    var params = Array();
    var pairs = location.search.substring(1).split("&");
    var redirect;

    for (i in pairs) {
      var pair = pairs[i].split("=");
      // translate parameters here
      switch (pair[0]) {
      case '':
        // ignore empty parameters
        break;
      case 's': // type
        // switch (pair[1]) {
        //   case 'type':
        //     redirect = 'cat';
        //     break;
        //   case 'subarea':
        //     redirect =
        // }
        break;
      case 'debugLevel':
        debugLevel = pair[1];
        // document.write('debugLevel is now ', debugLevel, '<p/>');
        break;
      default:
        name         = pair[0];
        value        = unescape(pair[1]);
        params[name] = value;
        if(debugLevel > 1 && name != 'description') {
          debugInfo.addValue(name, value);
        }
      }
    }
    return params;
  }
  var userParams    = collectUserParams();

};

PGS_mod_namespace();
