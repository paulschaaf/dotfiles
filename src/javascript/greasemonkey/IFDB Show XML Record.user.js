// ==UserScript==
// @name         IFDB Show XML Record
// @namespace    tag:paul.schaaf@gmail.com,2010-01-01
// @version      0.1
// @description  Adds a link to show the XML record to an IFDB entry
// @author       P.G. Schaaf
// @match        http*://ifdb.tads.org/viewgame*
// @grant        none
// @require      file:///home/pschaaf/src/javascript/greasemonkey/IFDB Show XML Record.user.js
// ==/UserScript==

(function() {
    'use strict';
    var link = document.createElement('a');
    link.href = document.location.href + '&ifiction';
    link.target = '_blank';
    link.text = 'XML Record';

    var detailsSection = document.getElementsByClassName("details")[0];
    detailsSection.appendChild(link);
})();