/*
allUsers = new Array(
  'aapplegate',   'aarlington', 'abaxter',     'aclinton',
  'admin',        'akim',       'alee',        'alevin',
  'amunoz',       'bbaker',     'bcarter',     'bfawcett',
  'bhamilton',    'bhearst',    'bhunter',     'bmalouin',
  'bnixon',       'btyler',     'bzimmerson',  'carkle',
  'cbeaumont',    'ccarley',    'cclark',      'ccraft',
  'chart',        'clevitt',    'cnorris',     'coppley',
  'cpayne',       'cshaw',      'csherman',    'ctersley',
  'dbush',        'ddederick',  'devans',      'dguest',
  'dhayes',       'dhenson',    'dmyers',      'dolsen',
  'edean',        'elee',       'emazuch',     'enyugen',
  'epasquale',    'erogers',    'fdonahue',    'fdwight',
  'fford',        'fkillian',   'fthomson',    'fwagner',
  'gearnhart',    'gickes',     'gkennedy',    'gloch',
  'gwang',        'hatley',     'hdienstfrey', 'hjohnson',
  'iharkin',      'ilang',      'inumin',      'jcalhoon',
  'jcoolidge',    'jcumberland','jdole',       'jegertson',
  'jfilipkowski', 'jhendricks', 'jjosloff',    'jnafferty',
  'jrandolph',    'jroberts',   'jruhl',       'jstreeter',
  'jwork',        'jyancy',     'kbrown',      'kraftly',
  'krice',        'ksharpton',  'kwinslow',    'ldonahue',
  'lfarrell',     'ljames',     'lquinn',      'ltaft',
  'lwhipple',     'mikeshaw',   'mking',       'mmaples',
  'mrickter',     'mspanbauer', 'ndrew',       'nmyrick',
  'ntamden',      'oyoung',     'pbenson',     'pflores',
  'pgebhardt',    'pgrimes',    'pharrison',   'pmansur',
  'pmoseley',     'pvance',     'rbarnes',     'rdavis',
  'repadmin',     'revans',     'rgregoire',   'rkerrey',
  'rmyette',      'rralston',   'rreagan',     'rwest',
  'sarthur',      'sbain',      'sbuchanan',   'sdunn',
  'sfarley',      'shenson',    'shoover',     'sjacobs',
  'slieberman',   'slincoln',   'spierce',     'ssmith',
  'struman',      'sulveling',  'svisor',      'tallen',
  'tcrawford',    'tedwards',   'tgarfield',   'tgrant',
  'tgunderson',   'tjohnson',   'tmazzon',     'tmcdermott',
  'tsanders',     'twilkie',    'twilson',     'vpignano',
  'wcummings',    'wdufraine',  'weisenhower', 'wgompers',
  'wkennison',    'wmann',      'wmotley',     'wroosevelt'
 );
*/

// ===========================================================
function GWServer(appName) {
  this.app      = appName;
  switch(this.app) {
    case 'ab':
      this.port = 8280;
      break;
    case 'bc':
      this.port = 8380;
      break;
    case 'cc':
      this.port = 8080;
      break;
    case 'pc':
      this.port = 8180;
      break;
    }
  this.users = Array(
    'su',
    'student01',
    'aapplegate',
    'bbaker',
    'ccraft',
    'gickes',
    'rbarnes');
}

GWServer.prototype.loginUrl = function (user) {
  return  'http://127.0.0.' + (user.charCodeAt(0) + user.charCodeAt(1))
    + ':' + this.port + '/'
    + this.app + '/Login.do?loginPassword=gw&loginName=' + user;
};

GWServer.prototype.loginLink = function (user) {
  return '<a target="_blank" href="' + this.loginUrl(user) + '">' + user + "</a>";
};

GWServer.prototype.writeTo = function (aDoc) {
  for each(user in this.users) {
    aDoc.write('<br/>' + this.loginLink(user));
  }
};

// ===========================================================
function collectUserParamsIn(params) {
  var pairs = location.search.substring(1).split("&");
  for (i in pairs) {
    var pair = pairs[i].split("=");
    // translate parameters here
    switch (pair[0]) {
      case "":
        // ignore empty parameters
        break;
      case "debugLevel":
        debugLevel = pair[1];
        //document.write('debugLevel is now ', debugLevel, '<p/>');
        break;
      default:
        params[pair[0]] = unescape(pair[1]);
    }
  }
  return params;
}

// ===========================================================
var userParams = Array();

collectUserParamsIn(userParams);

var app_name = userParams['app_name'];

var app_ids = (userParams['app_id'] == null)
                ? Array('ab', 'bc', 'cc', 'pc', 'px')
                : Array(userParams['app_id']);

document.write('<table><tr>');
for each (app_id in app_ids) {
  var server = new GWServer(app_id);
  document.write('<td width="22%"><h1>' + (app_name || app_id) + '</h1>');
  server.writeTo(document);
  document.write('</td>');
}
document.write('</tr></table>');
