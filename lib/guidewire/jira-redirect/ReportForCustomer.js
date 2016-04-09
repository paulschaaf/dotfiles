// -*- compile-command: (concat "svn commit -m ''; ssh satori 'cd " (file-name-directory buffer-file-name) " && svn update && rsync -avu --exclude=.svn . ~apache/html/jira-redirect'") -*-

actionField       = "customfield_10151";
customerNameField = "customfield_10060";

customers = new Array(
  'AMERCO',
  'Amica',
  'AXA Canada',
  'Canal',
  'CIGA',
  'CNA',
  'CNA Policy',
  'Dominion',
  'Donegal',
  'EDIC',
  'FCCI',
  'GAIC',
  'GEICO',
  'Hanover',
  'Hastings Mutual',
  'ICAT',
  'KFB Policy',
  'Kentucky Farm Bureau',
  'Liberty Mutual',
  'Liberty Surety',
  'MSA',
  'Merastar',
  'Montana State',
  'New Jersey Mfg Grp',
  'New Mexico Mutual',
  'Privilege',
  'Sentry',
  'Sequoia',
  'SUA',
  'Suncorp'
);

basicUrl                      = "http://jira/jira/secure/IssueNavigator.jspa";
debugLevel                    = 0;

defaultParams                 = Array();
defaultParams.mode            = "show";
defaultParams.reset           = true;
defaultParams.tempMax         = 120;

defaultParams.pid             = "10040&pid=10041&pid=10080";
defaultParams.status          = "1&status=3&status=4&status=5";
defaultParams["sorter/field"] = "issuekey&sorter/order=ASC&sorter/field=priority&sorter/order=DESC&sorter/field=status&sorter/order=ASC";

function collectUserParams(paramString) {
  params = Array();
  userParamPairs = paramString.split("&");
  for (i in userParamPairs) {
    pair = userParamPairs[i].split("=");
    // translate parameters here
    switch (pair[0]) {
    case "":
      // ignore empty parameters
      break;
    case "debugLevel":
      debugLevel = pair[1];
      document.write('debugLevel is now ', debugLevel, '<p/>');
      break;
    default:
      params[pair[0]] = pair[1];
    }
  }
  return params;
}

function debugWrite(level) {
  if ((level || 1) >= debugLevel) {
    arguments.shift();
    document.write(arguments);
  }
}

// If I add this as a function to Array, the iterators will treat this function as if it were a customer of the same name
function dump_array(array) {
  document.write('<hr/>');
  for (key in array) { document.write('<li>', key, ': ', array[key], '</li>'); }
  document.write('<hr/>');
}

userParams      = collectUserParams(location.search.substring(1));
if (debugLevel >= 1) { dump_array(params); }

function paramAndValue(key, value) { return "&" + key + "=" + value; }

function pageForCustomer(cust) {
  answer = basicUrl + "?";
  if (cust) {
    answer += paramAndValue(customerNameField, cust);
  }
  else {
    for (key in userParams) {
      answer += paramAndValue(key, userParams[key]);
    }
    for (key in defaultParams) {
      if (! userParams[key]) {
        answer += paramAndValue(key, defaultParams[key]);
      }
    }
  }
  return answer;
}

// if customer specified, don't render the list of links, just go straight to jira
if (userParams[customerNameField]) {
  window.location = pageForCustomer();
}
else {
  document.write('<h1>' + document.title + '</h1>');
  basicUrl = "";
}

function renderList() {
  for (idx in customers) {
    cust = customers[idx];
    document.write(
      '<li>' +
        '<a target="_blank" href="' + pageForCustomer(cust) + '">' + cust + '</a>' +
        '&nbsp;&nbsp;(<a target="_blank" href="' + pageForCustomer(cust) + '&view=excel-current" class="small">xls</a>)' +
        '</li>'
    );
  }
}



// proj_ffs
// resolution_unresolved
// resolution_verified
// customerNameField = "Liberty Mutual"
// status_open
// status_in_progress
// status_reopened
// status_resolved

// function sort_ascending_by(name) {
//    return "&sorter/field=" + name + "&sorter/order=ASC";
// }
// function sort_descending_by(name) {
//    return "&sorter/field=" + name + "&sorter/order=DESC";
// }

// sort_ascending_by(actionField)
// sort_ascending_by("resolution")
// sort_ascending_by("status")

// function pair(key, value) {
//    this.to_s  = function() {
//       return "&" + this.key + "=" + this.value
//    }

//    this.key   = key
//    this.value = value
// }

// Array.prototype.addKeyValue = function(key, value) {
//    this[key] = new pair(key, value)
//    return this[key]
// }

// Array.prototype.to_s = function() {
//    answer = ""
//    for (idx in this) {
//       answer += this[idx].to_s
//    }
//    return answer
// }

// function encode(name) { return name.replace(" ", "%20") }

// sort_ascending_by(actionField);
// sort_ascending_by(customerNameField);
// sort_ascending_by("issueKey");
// sort_descending_by("priority");
// sort_ascending_by("status");

// projects = new Array()
// projects.addKeyValue('cc',  10000)
// projects.addKeyValue('ffs', 10040)
// projects.addKeyValue('pg',  10041)
// projects.addKeyValue('ss',  10080)

// statuses = new Array()
// statuses.addKeyValue('open')
// statuses.addKeyValue('in_progress')
// statuses.addKeyValue('reopened')
// statuses.addKeyValue('resolved')
// statuses.addKeyValue('closed')

// resolution_unresolved         = "&resolution=-1";
// resolution_verified           = "&resolution=6";

// function status(code) {
//    return "&statusIds=" + code;
// }
// statuses                      = new Array();
// statuses.open                 = new status(1);
// statuses.in_progress          = new status(3);
// statuses.reopened             = new status(4);
// statuses.resolved             = new status(5);
// statuses.closed               = new status(6);

// updated_in_last_7_days        = "&updatedPrevious=604800000";
// updated_in_last_30_days       = "&updatedPrevious=2592000000";

