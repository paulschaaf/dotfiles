// ==UserScript==
// @name             Guidewire Application Login
// @namespace        tag:paul.schaaf@gmail.com,2009-03-07
// @version          5.0
// @description      Adds quick-login links to the login screen. If the hostname is "localhost" or "127.*" then each
//    link will go to a (reasonably) unique IP address in the loopback range (127.x.x.x). Because the browser session
//    cookies are filed under the hostname or IP address (whichever you use in the URL), this will allow you to have
//    several tabs in the same browser logged in as different users simultaneously.
// @run-at           document-end
// @match            http*://*/*Center.do
// @match            http*://*/*/*Center.do
// @match            http*://*/ContactManager.do
// @match            http*://*/*/ContactManager.do
// @copyright 2009+  P.G. Schaaf
// @require          https://cdnjs.cloudflare.com/ajax/libs/babel-standalone/6.18.2/babel.js
// @require          https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.16.0/polyfill.js
// @require          file:///home/pschaaf/src/javascript/greasemonkey/debugging.js
// @require          file:///Users/pschaaf/src/javascript/greasemonkey/debugging.js
// @require          file:///Users/pschaaf/src/javascript/greasemonkey/guidewire_application_lo.user.js
// @downloadURL      file:///Users/pschaaf/src/javascript/greasemonkey/guidewire_application_lo.user.js
// @updateURL        file:///Users/pschaaf/src/javascript/greasemonkey/guidewire_application_lo.user.js
// ==/UserScript==

// @require          https://github.com/paulschaaf/dotfiles/raw/32d53c378fa14c4e7335cca8a8408daba57e0c82/src/javascript/greasemonkey/guidewire_application_lo.user.js
// @require          https://github.com/paulschaaf/dotfiles/raw/32d53c378fa14c4e7335cca8a8408daba57e0c82/src/javascript/greasemonkey/debugging.js
// @downloadURL      https://github.com/paulschaaf/dotfiles/raw/32d53c378fa14c4e7335cca8a8408daba57e0c82/src/javascript/greasemonkey/guidewire_application_lo.user.js
// @updateURL        https://github.com/paulschaaf/dotfiles/raw/32d53c378fa14c4e7335cca8a8408daba57e0c82/src/javascript/greasemonkey/guidewire_application_lo.user.js

var debugLevel = 0;

var eachKeyAndValue = (hash, kvFunction) => {
  for (var key in hash) {
    if (hash.hasOwnProperty(key)) kvFunction(key, hash[key]);
  }
}

// == Style Sheet ==================================================
var css =
    '.pgs             { gw margin-left: auto; margin-right: auto; text-align: left; padding: 4px; } ' +
    '.rotate          { -webkit-transform: rotate(270deg); -ms-transform: rotate(270deg); -moz-transform: rotate(270deg); -o-transform: rotate(270deg); width: 20px; } ' +
    'tr#favorite      { border-bottom: thin solid black; } ' +
    '.linkGroupCell   { border-right: thin solid black; font-size: large; } ' +
    'td               { padding: 0px 3px; } ' +
    '#favorite        { font-weight: bold; } ' +
    'tr.pgs           { border-bottom: thin dotted lightgray; background-color: white; } ' +
    'tr.pgs:nth-child(odd) { background-color: LightYellow; } ' +
    'table.pgs        { empty-cells: show; border-collapse: collapse; border-spacing: 30px; } ' +
    '.wrapperCell     { border: 1px solid black; padding: 0px; } ' +
    '.wrapperTable    { margin-left: auto; margin-right: auto; } ' +
    '.userLogin       { font-weight: bold; border-right: thin solid black; } ' +
    '.linkGroup       { background-color: LightSkyBlue; } ';

if (debugLevel > 0) {
  css += '.insured   { border: thin solid black; }';
}

document.old_createElement = document.createElement;
document.createElement = tagName => {
  var element = document.old_createElement(tagName);
  element.className = 'pgs';
  return element;
};

var debugInfo = new DebugInfoHeader(debugLevel);
debugInfo.insert = info => document.getElementById('Login-table').insertAdjacentElement("beforebegin", info);

// ===========================================================
function GWClaimCenter() {
  this.specialEntryPoints = [
    'InternalTools',
    // 'CentipedeCacheInfo',
    // 'DatabaseStatistics',
    // 'DatabaseTableInfo',
    // 'DatabaseDistributionInfo',
    'InfoPages',
    // 'Profiler',
  ];

  var autoClaimants1 = ['Allen Robertson', 'Bill Kinman', 'Karen Egertson', 'Ray Newton', 'Robert Farley'];
  var autoClaimants2 = ['Brittany Turner', 'Larry Gamney', 'Lisa Shiu', 'Mark Henderson'];
  var propClaimants  = ['Paladin Financial', 'Western Farmers'];
  var wcClaimants    = ['Wright Construction'];

  this.claims = {
    'Allen Robertson': '235-53-365871',
    'Bill Kinman': '235-53-425891',
    'Brittany Turner': '235-53-373871',
    'Karen Egertson': '235-53-425892',
    'Larry Gamney': '235-53-373906',
    'Lisa Shiu': '235-53-373872',
    'Mark Henderson': '235-53-373870',
    'Paladin Financial': '426-24-366071',
    'Ray Newton': '235-53-365870',
    'Robert Farley': '235-53-365889',
    'TipTop Roofing': '312-36-368889',
    'Western Farmer\'s': '426-24-366070',
    'Wright Construction': '312-36-368870'
  };
  this.users = {
    su:         autoClaimants1,

    ssmith:     autoClaimants1.concat(autoClaimants2).sort(), // supervisor
    mmaples:    autoClaimants1.concat(autoClaimants2).sort(), // supervisor
    aapplegate: autoClaimants1,
    bbaker:     autoClaimants2,
    cclark:     autoClaimants1,
    ccraft:     autoClaimants2,

    wmotley:    wcClaimants, // supervisor
    gickes:     wcClaimants,

    carkle:     propClaimants, // supervisor
    rbarnes:    propClaimants,
  };

  // do all of this last
  this.userClaims = {};
  this.usernames = [];
  this.highestClaimCount = 0;

  var self = this;
  eachKeyAndValue(this.users, (username, insuredNames) => {
    self.usernames.push(username);
    var claimNumbers = [];

    self.highestClaimCount = Math.max(insuredNames.length, self.highestClaimCount);

    insuredNames.forEach(insured => {
      claimNumbers.push([insured, self.claims[insured]]);
    });

    self.userClaims[username] = claimNumbers;
  });
}

var cc = new GWClaimCenter();

var appCode     = document.location.pathname.match(/ab|bc|cc|pc|px/),
    isLocalHost = document.location.hostname.match(/^(127\.0\.0\.1|localhost)$/) !== null;

if (!appCode) {
  if (document.location.pathname.match("/BillingCenter.do$")) {
    appCode = "bc";
  }
  else if (document.location.pathname.match("/ClaimCenter.do$")) {
    appCode = "cc";
  }
  else if (document.location.pathname.match("/ContactManager.do$")) {
    appCode = "ab";
  }
  else if (document.location.pathname.match("/PolicyCenter.do$")) {
    appCode = "pc";
  }
  else if (document.location.pathname.match("/ExampleCenter.do$")) {
    appCode = "px";
  };
}
debugInfo
    .addValue('appCode', appCode)
    .addValue('isLocalHost', isLocalHost);


// ===========================================================
function GWServer() {
  this.app = '/' + appCode;
  this.loginLinksHtmlID = 'PGS_logins';
  this.isLocalHost = isLocalHost;
  this.openLinkInNewTab = false;
}

GWServer.prototype.hostnameForUser = user => {
  // Create a unique hostname for each user beginning with "127.0.0.", with the last part being an encoding of the
  // first two characters of the login username. This should give good enough uniqueness for our sample user names.
  var host = document.location.hostname;
  if (this.openLinkInNewTab && this.isLocalHost) {
    host = '127.0.0.' + (user.charCodeAt(0) + user.charCodeAt(1));
  }
  return host;
};

GWServer.prototype.hostForUser = user => document.location.protocol + '//'
      + this.hostnameForUser(user)
      + ':' + document.location.port;

GWServer.prototype.pathnameForUser = user => this.isLocalHost
      ? this.hostForUser(user)
      : ''; // use default relative pathname

GWServer.prototype.appUrlForUser = function (user) {
  return this.pathnameForUser(user) + this.app + '/';
};

GWServer.prototype.entryPointUrlForUser = function (user, entryPoint) {
  return this.appUrlForUser(user)
      + entryPoint + '.do?loginPassword=gw&loginName='
      + user;
};

GWServer.prototype.newLinkForUser = function (user, entryPoint, textContent, urlParams) {
  if (textContent == null) textContent = user;
  if (urlParams == null) urlParams = '';

  var url = this.entryPointUrlForUser(user, entryPoint);

  var link = document.createElement('a');
  link.href = url + urlParams;
  link.title = link.href;       // in case the status bar truncates the URL
  link.innerHTML = textContent;

  if (this.openLinkInNewTab && this.isLocalHost) link.target = '_blank';

  if (debugLevel === 1) {
    if (link.target !== "") {
      var span = document.createElement('span');
      span.appendChild(link);
      span.insertAdjacentText("afterbegin", '+');
      span.insertAdjacentText('beforeend', '+');
      link = span;
    }
  }
  else if (debugLevel >= 2) link.textContent = link.href;
  else if (debugLevel >= 3) link.textContent = link.outerHTML;

  return link;
};

GWServer.prototype.appendNewCellToRow = function (row, content, isHeaderCell) {
  var cellType = isHeaderCell ? 'th' : 'td';
  var cell = document.createElement(cellType);
  if (content != null) cell.appendChild(content);
  row.appendChild(cell);
  return cell;
};

GWServer.prototype.appendSpecialCellsToRow = function (user, row) {
  row.id = 'favorite';

  console.log('specialEntryPoints = ' + cc.specialEntryPoints);

  var self = this;
  cc.specialEntryPoints.forEach(entryPoint => {
    var link = self.newLinkForUser(user, entryPoint, entryPoint);
    var cell = self.appendNewCellToRow(row, link);
    cell.id = 'favorite';
  });
};

GWServer.prototype.newLinkRowForUser = function (user) {
  var row = document.createElement('tr'),
      self = this;

  var mainLoginCell = this.appendNewCellToRow(row, this.newLinkForUser(user, 'Login'));
  mainLoginCell.className = 'userLogin';

  cc.userClaims[user].forEach(insuredAndClaimNumber => {
    var insured = insuredAndClaimNumber[0];
    var link = self.newLinkForUser(
        user,
        'ClaimSummaryLink',
        insured.replace(' ', '&nbsp;'),
        '&claimNumber=' + insuredAndClaimNumber[1]
    );
    var insuredCell = self.appendNewCellToRow(row, link);
    insuredCell.className = 'insured';
    if (insured === 'Ray Newton') {
      insuredCell.id = 'favorite';
    }
  });

  if (user === 'su') this.appendSpecialCellsToRow(user, row);

  // fill out the rest of the row
  var fillerCell = document.createElement('td');
  fillerCell.colSpan = cc.highestClaimCount - row.childElementCount + 1;
  row.appendChild(fillerCell);

  return row;
};

GWServer.prototype.appendNewLabelCellToRow = row => {
  var label = document.createElement('p');
  label.className = 'rotate';
  label.innerHTML = this.openLinkInNewTab ? 'New&nbsp;Tab' : 'Same&nbsp;Tab';

  var labelCell = this.appendNewCellToRow(row, label, true);
  labelCell.className = 'linkGroupCell';
  return labelCell;
};

GWServer.prototype.addLinkGroupLabelToTable = (users, table) => {
  var labelRow = document.createElement('tr');
  labelRow.className = 'linkGroup';
  table.appendChild(labelRow);

  var labelCell = this.appendNewLabelCellToRow(labelRow);
  labelCell.rowSpan = users.length + 1;
};

GWServer.prototype.addLoginLinkRowsForUsersToTable = function (users, table) {
  if (this.isLocalHost) this.addLinkGroupLabelToTable(users, table);
  var self = this;
  users.forEach(user => {
    var row = self.newLinkRowForUser(user);
    table.appendChild(row);
  });
};

GWServer.prototype.loginLinkTableForUsers = function (users) {
  var linkTable = document.createElement('table');
  this.addLoginLinkRowsForUsersToTable(users, linkTable);

  var wrapperRow = document.createElement('tr');
  var wrapperCell = this.appendNewCellToRow(wrapperRow, linkTable);
  wrapperCell.className = 'wrapperCell';

  var wrapperTable = document.createElement('table');
  wrapperTable.className = 'wrapperTable';
  wrapperTable.appendChild(wrapperRow);
  return wrapperTable;
};

GWServer.prototype.loginLinksSection = function (users) {
  var container = document.createElement('span');

  container.appendChild(this.loginLinkTableForUsers(users));

  if (this.isLocalHost) {
    this.openLinkInNewTab = true;
    container.appendChild(this.loginLinkTableForUsers(users));
    container.appendChild(document.createElement('br'));
    container.appendChild(document.createElement('br'));
    this.openLinkInNewTab = false;
  }

  return container;
};


// ===========================================================

document.addLinksIfLoginScreen = function (triggerName, server) {
  if (document.loginLinksElem == null) {

    // don't use if product includes its own auto-login or data-load control
    if (document.getElementById('Login-LoginScreen-LoginLinksPanelSet') != null
        || document.getElementById('Login-LoginScreen-LoginDV-AutoLoginLV') != null
        || document.getElementById('quickLink') != null
        || document.getElementById('Login-LoginScreen-LoginDV-sampleData') != null) {
      console.log(triggerName + ': Skipping links because this already contains the quick login links LV');
      return;
    }

    var baseLoginFields = document.getElementById('Login-LoginScreen-5') // cc50
        || document.getElementById('Login-LoginScreen-LoginDV-0') // cc10
        || document.getElementById('Login-LoginScreen-LoginDV-1') // cc10
        || document.getElementById('Login-table') // cc9
        || document.getElementById('Login'); // cc8

    if (baseLoginFields == null) {
      console.log(triggerName + ': Skipping links because this is not the login screen');
    }
    else {
      console.log(triggerName + ': Adding links to the login screen');

      var pgsStyle = document.createElement('style')
      pgsStyle.innerHTML = css;

      var linksTable = document.createElement('div');
      linksTable.id = server.loginLinksHtmlID;
      linksTable.appendChild(pgsStyle);
      linksTable.appendChild(document.createElement('br'));
      linksTable.appendChild(server.loginLinksSection(cc.usernames));

      var parent = baseLoginFields;
      parent.appendChild(document.createElement('br'));
      parent.appendChild(linksTable);
      document.loginLinksElem = linksTable;
    }
  }
  else console.log(triggerName + ': The Login links are already shown.');
};

document.gw_server = new GWServer();

window.setTimeout(() => document.addLinksIfLoginScreen('setTimeout', document.gw_server));

console.log('Checking whether to enable the Login links.');

/*
  var login = document.getElementsByName('Login-LoginScreen-LoginDV-username')[0];
  var password = document.getElementsByName('Login-LoginScreen-LoginDV-password')[0];
  var button = document.getElementById('Login-LoginScreen-LoginDV-submit_inner');

  login.setRangeText('aapplegate');
  password.setRangeText('gw');
  button.click();
*/
