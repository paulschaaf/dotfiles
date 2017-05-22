// ==UserScript==
// @name           Guidewire Application Login
// @namespace      tag:paul.schaaf@gmail.com,2009-03-07
// @version 4.0
// @description    Adds quick-login links to the login screen.
// @run-at         document-end
// @match          http://*/bc/BillingCenter.do*
// @match          http://*/cc/ClaimCenter.do*
// @match          http://*/px/CommandCenter.do*
// @match          http://*/ab/ContactCenter.do*
// @match          http://*/px/ExampleCenter.do*
// @match          http://*/pc/PolicyCenter.do*
// @copyright 2009+ P.G. Schaaf
// @downloadURL    file:///C:/Users/pschaaf/lib/js/greasemonkey/guidewire_application_lo_user.js
// @updateURL      file:///C:/Users/pschaaf/lib/js/greasemonkey/guidewire_application_lo_user.js
// ==/UserScript==

// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// If the hostname in the login URL screen is "localhost" or "127.*" then
// each link will go to a (reasonably) unique IP address in the loopback
// range (127.0.0.1--127.255.255.255). Because the browser session cookies
// are filed under the hostname or IP address (whichever you use in the URL),
// this will allow you to have several tabs in the same browser logged in
// as different users simultaneously.
//
// If the login screen hostname is NOT "localhost" or "127.*" then the links
// do not use custom hostnames.
//
// -------------------------------------------------------------------------
//
// This is a Greasemonkey user script.
// 06/09/14: Local file reload is working again.
// 05/06/14: It used to be possible for Tampermonkey to load this script from a local file,
// but that seems broken now (see https://www.google.com/search?q=tampermonkey+require+local+file&oq=tampermonkey+require+lo&aqs=chrome.1.69i57j0.7270j0j7&sourceid=chrome&es_sm=93&ie=UTF-8)
//
// -------------------------------------------------------------------------

var debugLevel = 0;

function eachKeyAndValue(hash, kvFunction) {
  for (var key in hash) {
    if (hash.hasOwnProperty(key)) kvFunction(key, hash[key]);
  }
}

// ===========================================================
// Style Sheet
var css = '.pgs                  { font-size: small; margin-left: auto; margin-right: auto; text-align: left; } ' +
  '.rotate               { -webkit-transform: rotate(270deg); -ms-transform: rotate(270deg); ' +
  '-moz-transform: rotate(270deg);  -o-transform: rotate(270deg); ' +
  'width: 20px; } ' +
  'tr#favorite           { border-bottom: thin solid black; } ' +
  '.linkGroupCell        { border-right: thin solid black; font-size: large; } ' +
  'td                    { padding: 0px 3px; } '+
  '#favorite             { font-weight: bold; } '+
  'tr.pgs                { border-bottom: thin dotted lightgray; background-color: white; } '+
  'tr.pgs:nth-child(odd) { background-color: LightYellow; } '+
  'table.pgs             { empty-cells: show; border-collapse: collapse; } ' +
  '.wrapperCell          { border: 1px solid black; padding: 0px; } ' +
  '.wrapperTable         { margin-left: auto; margin-right: auto; } ' +
  '.userLogin            { font-weight: bold; border-right: thin solid black; } ' +
  '.linkGroup            { background-color: LightSkyBlue; } ';

if (debugLevel > 0) {
  css += '.insured   { border: thin solid black; }';
}

document.old_createElement = document.createElement;
document.createElement = function(tagName) {
  var element = document.old_createElement(tagName);
  element.className = 'pgs';
  return element;
}

// ===========================================================
function DebugInfoHeader(debugLevel) {
  if      (navigator.appName.match(/Microsoft/)) this.browser = 'msie';
  else if (navigator.vendor.match(/Google/))     this.browser = 'chrome';
  else                                           this.browser = 'firefox';

  var self = this;
  this.debugLevel = debugLevel;
  this.text       = '';

  if (this.debugLevel > 0) window.setTimeout(function() { self.show(); });

  this.add = function(message) {
    this.text += message + '<br/>';
    return this;
  };

  this.addValue = function(name, value) {
    return this.add(name + ' == ' + value);
  };

  this.addValuesOf = function(objName, obj, propNames) {
    propNames.forEach(function(propName) {
                        self.addValue(objName + '.' + propName, obj[propName]);
                      });
    return this;
  };

  this.show = function() {
    if (this.debugLevel > 1 || (this.debugLevel == 1 && this.text.length > 0)) {
      var debugElem = document.createElement('h1');
      var html = '<div id="PGS_debug" style="'
        + 'margin: 0 auto 0 auto;     border-bottom: 1px solid #000000;'
        + 'margin-bottom: 5px;        font-size: large;'
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

      debugElem.innerHTML = html + this.text + '</p>' + '</div>';

      if (this.browser == 'msie') document.insertBefore(debugElem, document.firstChild);
      else document.body.insertBefore(debugElem, document.body.firstChild);
    }
  };

}

var debugInfo = new DebugInfoHeader(debugLevel);


// ===========================================================
function GWClaimCenter() {
  this.specialEntryPoints = [
    'InternalTools'
    //            , 'CentipedeCacheInfo'
    //            , 'DatabaseStatistics'
    //            , 'DatabaseTableInfo'
    //            , 'DatabaseDistributionInfo'
    //            , 'InfoPages'
    //            , 'Profiler'
  ];

  this.claims = {
    'Allen Robertson':     '235-53-365871',
    'Bill Kinman':         '235-53-425891',
    'Brittany Turner':     '235-53-373871',
    'Karen Egertson':      '235-53-425892',
    'Larry Gamney':        '235-53-373906',
    'Lisa Shiu':           '235-53-373872',
    'Mark Henderson':      '235-53-373870',
    'Paladin Financial':   '426-24-366071',
    'Ray Newton':          '235-53-365870',
    'Robert Farley':       '235-53-365889',
    'TipTop Roofing':      '312-36-368889',
    'Western Farmer\'s':   '426-24-366070',
    'Wright Construction': '312-36-368870'
  };
  this.users  = {
    su:         [],
    aapplegate: ['Allen Robertson', 'Bill Kinman', 'Karen Egertson', 'Ray Newton', 'Robert Farley'], // AUTO
    bbaker:     ['Brittany Turner', 'Larry Gamney', 'Lisa Shiu', 'Mark Henderson'], // AUTO
    cclark:     [], // AUTO clerical user
    ccraft:     [], // AUTO

    gickes:     ['Wright Construction'], // WC
    //        kwinslow:   [], // AUTO
    rbarnes:    ['Paladin Financial', 'Western Farmer\'s'] // PROP
  };

  // now the supervisors
  this.users['charcle'] = this.users['rbarnes'];

  this.users['ssmith'] = []
    .concat(this.users['aapplegate'])
    .concat(this.users['bbaker'])
    .concat(this.users['cclark'])
    .concat(this.users['ccraft'])
    .sort();

  this.users['mmaples'] = this.users['ssmith'];
  this.users['wmotley'] = this.users['gickes'];

  // do all of this last
  this.userClaims = {};
  this.usernames  = [];
  this.highestClaimCount = 0;

  var self = this;
  eachKeyAndValue(this.users, function(username, insuredNames) {
                    self.usernames.push(username);
                    var claimNumbers = [];

                    self.highestClaimCount = Math.max(insuredNames.length, self.highestClaimCount);

                    insuredNames.forEach(function(insured) {
                                           claimNumbers.push([insured, self.claims[insured]]);
                                         });

                    self.userClaims[username] = claimNumbers;
                  });
}

var cc = new GWClaimCenter();

var appCode     = document.location.pathname.match(/ab|bc|cc|pc|px/),
isLocalHost = document.location.hostname.match(/^(127\.0\.0\.1|localhost)$/) != null;

debugInfo.addValue('appCode',     appCode)
  .addValue('isLocalHost', isLocalHost);


// ===========================================================
function GWServer() {
  this.app              = '/' + appCode;
  this.loginLinksHtmlID = 'PGS_logins';
  this.isLocalHost      = isLocalHost;
  this.openLinkInNewTab = false;
}

GWServer.prototype.hostnameForUser = function (user) {
  // Create a unique hostname for each user beginning with "127.0.0.", with the last part being an encoding of the
  // first two characters of the login username. This should give good enough uniqueness for our sample user names.
  var host = document.location.hostname;
  if (this.openLinkInNewTab == true && this.isLocalHost) {
    host = '127.0.0.' + (user.charCodeAt(0)+user.charCodeAt(1));
  }
  return host;
};

GWServer.prototype.hostForUser = function(user) {
  return  document.location.protocol + '//'
    + this.hostnameForUser(user)
    + ':' + document.location.port;
};

GWServer.prototype.pathnameForUser = function(user) {
  return this.isLocalHost
    ? this.hostForUser(user)
    : '';   // use default relative pathname
};

GWServer.prototype.appUrlForUser = function(user) {
  return this.pathnameForUser(user) + this.app + '/';
};

GWServer.prototype.entryPointUrlForUser = function(user, entryPoint) {
  return this.appUrlForUser(user)
    + entryPoint + '.do?loginPassword=gw&loginName='
    + user;
};

GWServer.prototype.newLinkForUser = function(user, entryPoint, textContent, urlParams) {
  if (textContent == null) textContent = user;
  if (urlParams   == null) urlParams   = '';

  var url = this.entryPointUrlForUser(user, entryPoint);

  var link = document.createElement('a');
  link.href             = url + urlParams;
  link.title            = link.href;       // in case the status bar truncates the URL
  link.innerHTML        = textContent;

  if (this.openLinkInNewTab==true && this.isLocalHost) link.target = '_blank';

  if (debugLevel == 1) {
    if (link.target != "") {
      var span = document.createElement('span');
      span.appendChild(link);
      span.insertAdjacentText('afterBegin', '+');
      span.insertAdjacentText('beforeEnd',  '+');
      link = span;
    }
  }
  else if (debugLevel >= 2) link.textContent = link.href;
  else if (debugLevel >= 3) link.textContent = link.outerHTML;

  return link;
};

GWServer.prototype.appendNewCellToRow = function(row, content, isHeaderCell) {
  var cellType = isHeaderCell == true ? 'th' : 'td';
  var cell = document.createElement(cellType);
  if (content != null) cell.appendChild(content);
  row.appendChild(cell);
  return cell;
};

GWServer.prototype.appendSpecialCellsToRow = function(user, row) {
  row.id = 'favorite';

  console.log('specialEntryPoints = ' + cc.specialEntryPoints);

  for (var i in cc.specialEntryPoints) {
    var entryPoint = cc.specialEntryPoints[i];
    var link = this.newLinkForUser(user, entryPoint, entryPoint);
    var cell = this.appendNewCellToRow(row, link);
    cell.id = 'favorite';
  }
};

GWServer.prototype.newLinkRowForUser = function(user) {
  var row  = document.createElement('tr'),
  self = this;

  var mainLoginCell = this.appendNewCellToRow(row, this.newLinkForUser(user, 'Login'));
  mainLoginCell.className = 'userLogin';

  if (user == 'su') this.appendSpecialCellsToRow(user, row);

  cc.userClaims[user].forEach(function(insuredAndClaimNumber) {
                                var insured = insuredAndClaimNumber[0];
                                var link = self.newLinkForUser(
                                  user,
                                  'ClaimSummaryLink',
                                  insured.replace(' ', '&nbsp;'),
                                  '&claimNumber=' + insuredAndClaimNumber[1]
                                );
                                var insuredCell = self.appendNewCellToRow(row, link);
                                insuredCell.className = 'insured';
                                if (insured == 'Ray Newton') {
                                  insuredCell.id = 'favorite';
                                }

                              });

  // fill out the rest of the row
  var fillerCell = document.createElement('td');
  fillerCell.colSpan = cc.highestClaimCount - row.childElementCount + 1;
  row.appendChild(fillerCell);

  return row;
};

GWServer.prototype.appendNewLabelCellToRow = function(row) {
  var label           = document.createElement('p');
  label.className     = 'rotate';
  label.innerHTML     = this.openLinkInNewTab == true ? 'New&nbsp;Tab' : 'Same&nbsp;Tab';

  var labelCell       = this.appendNewCellToRow(row, label, true);
  labelCell.className = 'linkGroupCell';
  return labelCell;
};

GWServer.prototype.addLinkGroupLabelToTable = function(users, table) {
  var labelRow       = document.createElement('tr');
  labelRow.className = 'linkGroup';
  table.appendChild(labelRow);

  var labelCell      = this.appendNewLabelCellToRow(labelRow);
  labelCell.rowSpan  = users.length + 1;
};

GWServer.prototype.addLoginLinkRowsForUsersToTable = function(users, table) {
  if (this.isLocalHost) this.addLinkGroupLabelToTable(users, table);
  var self = this;
  for (var i in users) {
    var user = users[i];
    var row  = self.newLinkRowForUser(user);
    table.appendChild(row);
  }
};

GWServer.prototype.loginLinkTableForUsers = function(users) {
  var linkTable = document.createElement('table');
  this.addLoginLinkRowsForUsersToTable(users, linkTable);

  var wrapperRow         = document.createElement('tr');
  var wrapperCell        = this.appendNewCellToRow(wrapperRow, linkTable);
  wrapperCell.className  = 'wrapperCell';

  var wrapperTable       = document.createElement('table');
  wrapperTable.className = 'wrapperTable';
  wrapperTable.appendChild(wrapperRow);
  return wrapperTable;
};

GWServer.prototype.loginLinksSection = function(users) {
  var container = document.createElement('span');

  if (this.isLocalHost) {
    this.openLinkInNewTab = true;
    container.appendChild(this.loginLinkTableForUsers(users));
    container.appendChild(document.createElement('br'));
    container.appendChild(document.createElement('br'));
    this.openLinkInNewTab = false;
  }
  container.appendChild(this.loginLinkTableForUsers(users));
  return container;
};


// ===========================================================

document.addLinksIfLoginScreen = function(triggerName, server) {
  if (document.loginLinksElem == null) {
    var baseLoginFields = document.getElementById('Login');
    if (baseLoginFields == null) {
      console.log(triggerName + ': Skipping links because this is not the login screen');
    }
    else {
      console.log(triggerName + ': Adding links to the login screen');

      var pgsStyle = document.createElement('style');
      pgsStyle.innerHTML = css;

      var linksTable = document.createElement('div');
      linksTable.id  =  server.loginLinksHtmlID;
      linksTable.appendChild(pgsStyle);
      linksTable.appendChild(document.createElement('br'));
      linksTable.appendChild(server.loginLinksSection(cc.usernames));

      var parent = baseLoginFields.parentElement;
      parent.appendChild(document.createElement('br'));
      parent.appendChild(linksTable);
      document.loginLinksElem = linksTable;
    }
  }
  else console.log(triggerName + ': The Login links are already shown.');
};

document.gw_server = new GWServer();

window.setTimeout(function() {
                    document.addLinksIfLoginScreen('setTimeout', document.gw_server);
                  });

//var oldOnLoad = window.onload;
//window.onload = function() {
//    if (oldOnLoad != null) oldOnLoad();
//    document.addLinksIfLoginScreen('onload');
//}

//var oldOnResize = window.onresize;
//window.onresize = function() {
//    if (oldOnResize != null) oldOnResize();
//    document.addLinksIfLoginScreen('onresize');
//}