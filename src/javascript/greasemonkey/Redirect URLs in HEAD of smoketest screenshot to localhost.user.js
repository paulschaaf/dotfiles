// ==UserScript==
// @name         Redirect URLs in HEAD of smoketest screenshot to localhost
// @namespace    http://tampermonkey.net/
// @version      2017.11.16
// @author       P.G. Schaaf
// @match        http://thfiles/autofiles/results/*html
// ==/UserScript==

(function() {
    'use strict';
    var baseUrl = 'http://localhost:8090/cc/';
    var baseElement = document.evaluate('//base[@href]', document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
    baseElement.href = baseElement.href.replace(/^.*\/cc\//, baseUrl);

    var icon = document.createElement("link");
    icon.setAttribute("rel", "shortcut icon");
    icon.setAttribute("href", "img/app/favicon.ico");
    icon.setAttribute("type", "image/x-icon");
    document.head.appendChild(icon);
    document.head.innerHTML = document.head.innerHTML;
})();