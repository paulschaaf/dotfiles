// ==UserScript==
// @name         IFDB Show XML Record
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  try to take over the world!
// @author       You
// @match        http://ifdb.tads.org/*
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    var link = document.createElement('a');
    link.href = document.location.href + '&ifiction';
    link.target = '_blank';
    link.text = 'XML Record';
//    link.protocol = "view-source";

    var detailsSection = document.getElementsByClassName("details")[0];
    detailsSection.appendChild(link);
})();