// ==UserScript==
// @name         Copy stacktrace from TH
// @namespace    http://tampermonkey.net/
// @version      2017.11.27
// @author       P.G. Schaaf
// @match        https://th.guidewire.com/hippo/ToolsHarness*
// @grant        GM_setClipboard
// ==/UserScript==

(function() {
    'use strict';
    var fadeInMs = 500;

    var stackTraceLabelXPath = '//span[text()="Stack Trace "]';
    var getSingleNodeWithXPath = function(xPath) {
        return document.evaluate(xPath, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
    };

    var makeStackIntoButton = function () {
        var stackTraceLabel = getSingleNodeWithXPath(stackTraceLabelXPath);
        // Only display if the unedited stack trace label appears on the page.
        if (stackTraceLabel !== null) {
            var stackTraceNode = getSingleNodeWithXPath(stackTraceLabelXPath + '/../following-sibling::div'); // save before renaming button
            stackTraceLabel.setAttribute("class", "button");
            stackTraceLabel.innerText = "Copy Stack Trace";
            stackTraceLabel.onclick = function () {
                stackTraceLabel.setAttribute("style", "opacity: 0.1; transition: opacity 0.8s;");
                setTimeout(function () {stackTraceLabel.style.opacity = 1;}, fadeInMs);
                GM_setClipboard(stackTraceNode.innerText);
            };
            document.pgs_StackTraceLabel = stackTraceLabel;  // make it easily obtainable (for debugging purposes)
        }
    };

    // after every click (e.g. on the Prev or Next button) redraw the copy button
    document.onclick = function() {
        // We can't attempt this change until AJAX finishes rendering the
        // screen content, so as a hack we'll pause a bit before trying
        setTimeout(makeStackIntoButton, 500);
    };

    // don't require an additional click to initially display the button
    document.head.click();
})();