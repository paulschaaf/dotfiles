// ==UserScript==
// @name           Craigslist Auto-Poster
// @namespace      tag:paul.schaaf@gmail.com,2009-03-07
// @description    Automate postings to Craigslist from URL parameters
// @match          https://post.craigslist.org/sfo/*
// ==/UserScript==

// Craigslist Auto-Poster user script
// version 1.1 BETA!
// 2009/08/07
// Copyright (c) 2009-11 P.G. Schaaf (paul.schaaf@gmail.com)
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// -------------------------------------------------------------------------
//
// This is a Greasemonkey user script.
//
// To install, you need Greasemonkey: http://greasemonkey.mozdev.org/
// Then restart Firefox and revisit this script.
// Under Tools, there will be a new menu item to "Install User Script".
// Accept the default configuration and install.
//
// To uninstall, go to Tools/Manage User Scripts, select this script,
// and click Uninstall.
//
// -------------------------------------------------------------------------

var debugLevel = 2;

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

function urlParams() {
  var name;
  var pair;
  var pairs = location.search.substring(1).split("&");
  var params = Array();
  var value;

  for(i in pairs) {
    pair         = pairs[i].split("=");
    name         = pair[0];
    value        = unescape(pair[1]);
    params[name] = value;
  }
  return params;
}

function collectUserParams() {
  var params = urlParams();
  var value;

  for (name in params) {
    value = params[name];
    // translate parameters here
    switch (name) {
    case '':
      // ignore empty parameters
      break;
    case 'debugLevel':
      debugLevel = value;
      // document.write('debugLevel is now ', debugLevel, '<p/>');
      break;
    default:
      if(debugLevel > 1 && name != 'description') {
        debugInfo.addValue(name, value);
      }
    }
  }
  return params;
}

/*
 * s=type, fs
 * s=cat, <cat number>
 * s=subarea, 3
 * s=hood, 54
 * s=edit, ...
 */

function processParams() {
  var params = urlParams();
  var value;

  for (name in params) {
    value = params[name];
    // translate parameters here
    switch (name) {
    case '':
      // ignore empty parameters
      break;
    case 's':
      switch (value) {
      case 'type':
        break;
      case 'cat':
        break;
      case 'subarea':
        break;
      case 'hood':
        break;
      case 'edit':
        break;
      }
    case 'debugLevel':
      debugLevel = value;
      // document.write('debugLevel is now ', debugLevel, '<p/>');
      break;
    default:
      if(debugLevel > 1 && name != 'description') {
        debugInfo.addValue(name, value);
      }
    }
  }
  return params;
}

// ===========================================================
/*
function showAllHiddenFields() {
  var snapHidden = document.evaluate("//input[@type='hidden']",
                                     document,
                                     null,
                                     XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE,
                                     null);

  for (var i = snapHidden.snapshotLength -1; i>=0; i--){
    var elmHidden = snapHidden.snapshotItem(i);
    debugInfo.add('Hidden field: ' + elmHidden.id);
    elmHidden.stype.MozOutline = '1px dashed #666';
    elmHidden.setAttribute('type', 'text');
    elmHidden.type = 'text';
    elmHidden.title = 'Hidden field "' +
      (elmHidden.name || elmHidden.id) + '"';
  }
}
*/

/*
 * Click post leads to
 *    https://post.craigslist.org/k/WFYL00Z34BGgCrSsiaJ75Q/t71ud?s=type
 * radio fs
 *    https://post.craigslist.org/k/WFYL00Z34BGgCrSsiaJ75Q/t71ud?s=cat
 * User select category
 *    https://post.craigslist.org/k/WFYL00Z34BGgCrSsiaJ75Q/t71ud?s=subarea
 * radio 3
 *    https://post.craigslist.org/k/WFYL00Z34BGgCrSsiaJ75Q/t71ud?s=hood
 * radio 54
 *    https://post.craigslist.org/k/WFYL00Z34BGgCrSsiaJ75Q/t71ud?s=edit
 */


// ===========================================================
// ==== Find Fields on Form
var titleId       = null;
var priceId       = null;
var descriptionId = null;
var imageButtonId = null;

//var picId1        = null;

var loggedIn = false;

// priceId is the last text field, descriptionId is the only textarea field
for(i=0; i < thisForm.length; i++) {
  var elem = thisForm[i];
  //debugInfo.add('elem['+i+'].type == '+elem.type);
  switch (elem.type) {
    // case 'button':
    //   if(imageButtonId == null) {
    //     imageButtonId = i;
    //     thisForm[imageButtonId].click();
    //   }
    //   break;
    // case 'hidden':
    //   if(elem.value == 'paul.in.fremont@gmail.com') {
    //     loggedIn = true;
    //   }
    //   break;
    case 'radio':
    if (elem.value == 'fs'
        || elem.value == ''
       ) elem.click();
      break;
    case 'text':
      // first text field is the title, second is the price
      if(titleId == null) {
        titleId = i;
      }
      else {
        priceId = i;
      }
      break;
    case 'textarea':
      descriptionId = i;
      break;
    // case 'file':
    //   picId1 = i -1;
    //   break;
    default:
      break;
  }
}

(! loggedIn) && debugInfo.add('<blink>##### YOU ARE NOT LOGGED IN! #####</blink>');
// ===========================================================


// var imageButtonId = descriptionId + 1;
var picId1        = imageButtonId + 2;
var picId2        = picId1 + 2;
var picId3        = picId2 + 2;
var picId4        = picId3 + 2;

var urlParams    = collectUserParams();

// debugInfo.addValue('titleId',       titleId);
// debugInfo.addValue('priceId',       priceId);
// debugInfo.addValue('descriptionId', descriptionId);
//debugInfo.addValue('imageButtonId', imageButtonId);
// debugInfo.addValue('picId1',        picId1);
// debugInfo.addValue('picId2',        picId2);
// debugInfo.addValue('picId3',        picId3);
// debugInfo.addValue('picId4',        picId4);

if(urlParams['title'] != null) {

  // Unless/until I figure out how to auto-fill the filenames I'll add
  // them to the description area so I can cut and paste them manually
  var filenames = '';
  for(picNum=1; picNum<=4; picNum++) {
    var picId = 'pic' + (picNum-1);
    if (urlParams[picId] != null ) {
      filenames += ('\n' + urlParams[picId]);
    }
  }

  thisForm[titleId].value       = urlParams['title'];
  if(urlParams['price'] != null) {
    thisForm[priceId].value     = urlParams['price'];
  }
  thisForm[descriptionId].value = urlParams['description'] + filenames;

  // expand the 'attach image' section
  thisForm[imageButtonId].click();
  thisForm['imgfile1'].value = urlParams['pic0'];

  // debugInfo.addValue('num of buttons' + buttons.snapshotLength);

  // var button;
  // for(var btn=0; btn < buttons.snapshotLength; btn++) {
  //   button = buttons.snapshotItem(btn);
  //   debugInfo.addValue('btn[' + btn + ']', button.value);
  //   button.select();
  // }
}
  // =================================
  // =================================
  // =================================
  // =================================
  // Try to autofill the picture boxes
  // debugInfo.add('id of imageButtonId   = ' + thisForm[imageButtonId].id);
  // debugInfo.add('id of imageButtonId+1 = ' + thisForm[imageButtonId+1].id);
  // debugInfo.add('id of imageButtonId+2 = ' + thisForm[imageButtonId+2].id);

  // debugInfo.add('id of picId1 = ' + thisForm[picId1].id);

  //thisForm[pickId1].normalize();
  //thisForm[pickId1].parentNode().parentNode().click();
  //thisForm[pickId1+2].click();
  //thisForm[pickId1+3].select();
  //thisForm[pickId1+2].attr('src','C:\Documents and Settings\pschaaf\local\sync\images\photos\craigs-list\Princess House\Bear.jpg');
