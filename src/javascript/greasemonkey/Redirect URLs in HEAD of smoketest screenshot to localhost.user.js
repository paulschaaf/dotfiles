// ==UserScript==
// @name         Redirect URLs in HEAD of smoketest screenshot to localhost.
// @description  First start a mini webserver in your product installation root on port 8091 (e.g. "ruby -run -ehttpd ./app-cc/cc-run/build/idea/webapp -p8091").
// @namespace    tag:paul.schaaf@gmail.com,2010-01-01
// @version      2018.01.03
// @author       P.G. Schaaf
// @match        http://thfiles/autofiles/results/*html
// @require      file:///home/pschaaf/src/javascript/greasemonkey/Redirect URLs in HEAD of smoketest screenshot to localhost.user.js
// @require      file:///home/pschaaf/src/javascript/greasemonkey/debugging.js
// ==/UserScript==

(function() {
    'use strict';

    var baseUrl = 'http://localhost:8091/';
    var baseElement = document.evaluate('//base[@href]', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
    baseElement.href = baseElement.href.replace(/^.*\/cc\//, baseUrl);

    // force a redraw (the intermediate variable suppresses the compiler warning about reassigning variable to itself)
    var myHead = document.head;
    myHead.innerHTML = document.head.innerHTML;
})();
