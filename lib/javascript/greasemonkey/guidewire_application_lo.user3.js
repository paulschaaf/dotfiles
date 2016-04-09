// ==UserScript==
// @name           Guidewire Application Login
// @namespace      tag:paul.schaaf@gmail.com,2009-03-07
// @version 3.0
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
//
// -------------------------------------------------------------------------

var debugLevel = 2;

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
    };

    this.addValue = function(name, value) {
        this.add(name + ' == ' + value);
    };

    this.addValuesOf = function(obj, objName, propNames) {
        propNames.forEach(function(propName) {
            self.addValue(objName + '.' + propName, obj[propName]);
        });
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

            if (this.debugLevel > 3) {
                this.addValuesOf(navigator, 'navigator', navigator);
            }
            else if (this.debugLevel > 2) {
                this.addValuesOf(
                    navigator,
                    'navigator',
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
function GWServer() {
    this.app              = '/' + document.location.pathname.match(/ab|bc|cc|pc|px/);
    this.loginLinksHtmlID = 'PGS_logins';
    this.isLocalHost      = document.location.hostname.match(/^(127\.0\.0\.1|localhost)$/) != null;
    this.openLinkInNewTab = false;

    this.users = [
        'su',
        'aapplegate',
        'bbaker',
        'ccraft',
        'gickes',
        'ibelt',
        'kwinslow',
        'rbarnes',
        'ssmith',
        'mmaples'
    ];
    debugInfo.addValue('gw_server.app',              this.app);
    debugInfo.addValue('gw_server.isLocalHost',      this.isLocalHost);

    this.hostnameForUser = function (user) {
        // Create a unique hostname for each user beginning with "127.0.0.", with the last part being an encoding of the
        // first two characters of the login username. This should give good enough uniqueness for our sample user names.
        var host = document.location.hostname;
        if (this.openLinkInNewTab == true && this.isLocalHost) {
            host = '127.0.0.' + (user.charCodeAt(0)+user.charCodeAt(1));
        }
        return host;
    };
}

//GWServer.prototype.hostnameForUser = function (user) {
//    // Create a unique hostname for each user beginning with "127.0.0.", with the last part being an encoding of the
//    // first two characters of the login username. This should give good enough uniqueness for our sample user names.
//    var host = document.location.hostname;
//    if (this.openLinkInNewTab == true && this.isLocalHost) {
//        host = '127.0.0.' + (user.charCodeAt(0)+user.charCodeAt(1));
//    }
//    return host;
//};

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

GWServer.prototype.entryPointUrlForUser = function(entryPoint, user) {
    return this.appUrlForUser(user)
        + entryPoint + '.do?loginPassword=gw&loginName='
        + user;
};

GWServer.prototype.new_loginLinkForUser = function(user) {
    var link = document.createElement('a');

    // if it's localhost, open in a new window
    if (this.openLinkInNewTab==true && this.isLocalHost) link['target'] = '_blank';

    var url = this.entryPointUrlForUser('Login', user);
    link['href']  = url;
    link['title'] = url;     // in case status bar truncates URL

    link.textContent = user;

    if (debugLevel == 1) {
        if (link['target'] != "") {
            var span = document.createElement('span');
            span.appendChild(link);
            span.insertAdjacentText('beforeEnd', ' (@' + link['target'] + ')');
            link = span;
        }
    }
    else if (debugLevel >= 2) link.textContent = link['href'];
    else if (debugLevel >= 3) link.textContent = link.outerHTML;

    return link;
};

GWServer.prototype.loginLinksForUsers = function() {
    var links = document.createElement('p'),
        length = this.users.length;

    links['style'] = 'margin: 10px 5%;';
    if (this.isLocalHost) {
        var tabSection = document.createElement('b');
        tabSection.textContent = (this.openLinkInNewTab==true ? 'New' : 'Same') + ' Tab: ';
        links.appendChild(tabSection);
    }

    for (var i=0; i < this.users.length; i++) {
        if (i > 0) links.insertAdjacentText('beforeEnd', ', ');
        links.appendChild(this.new_loginLinkForUser(this.users[i]));
    }
    return links;
};

GWServer.prototype.loginLinks = function() {
    var html = document.createElement('span'),
        link = document.createElement('a');

    link['href'] = this.entryPointUrlForUser('InternalTools', 'su', false);
    link.textContent = 'InternalTools';
    html.appendChild(link);
    html.appendChild(this.loginLinksForUsers());
    if (this.isLocalHost) {
        this.openLinkInNewTab = true;
        html.appendChild(this.loginLinksForUsers());
    }
    return html;
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

            var linksTable = document.createElement('div');
            linksTable['id']    =  server.loginLinksHtmlID;
            linksTable['style'] = 'margin: 0px 15%; border: 1px solid #000000; background-color: white;';
            linksTable.appendChild(document.createElement('br'));
            linksTable.appendChild(server.loginLinks());

            baseLoginFields.appendChild(linksTable);
            document.loginLinksElem = linksTable;
        }
    }
    else console.log(triggerName + ': The Login links are already shown.');
};

var gw_server = new GWServer();

window.setTimeout(function() {
    document.addLinksIfLoginScreen('setTimeout', gw_server);
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
