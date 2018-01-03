// ==UserScript==
// @name         Redirect URLs in HEAD of smoketest screenshot to localhost
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

    // force a redraw (use an intermediate variable as a hack to suppress compiler warning about reassigning variable to itself)
    var myHead = document.head;
    myHead.innerHTML = document.head.innerHTML;
})();
