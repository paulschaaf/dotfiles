// ==UserScript==
// @name         Copy stacktrace from TH
// @namespace    tag:paul.schaaf@gmail.com,2010-01-01
// @version      2017.12.06
// @author       P.G. Schaaf
// @match        https://th.guidewire.com/hippo/ToolsHarness*
// @grant        GM_setClipboard
// @require      file:///home/pschaaf/src/javascript/greasemonkey/Copy stacktrace from TH.user.js
// ==/UserScript==

document.pgs_getSingleNodeWithXPath = function (xPath) {
  return document.evaluate(xPath, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
};

(function () {
  'use strict';
  var fadeInMs = 500;

  var stackTraceLabelXPath = '//span[text()="Stack Trace "]';

  var makeStackIntoButton = function () {
    var stackTraceLabel = document.pgs_getSingleNodeWithXPath(stackTraceLabelXPath);
    // Only display if the unedited stack trace label appears on the page.
    if (stackTraceLabel) {
      // var testName = document.pgs_getSingleNodeWithXPath('//span[text()="Test Name "]/following-sibling::span').innerText;

      if (document.pgs_StackContent && document.pgs_StackContent.parentNode) {
        document.pgs_StackContent.parentNode.removeChild(document.pgs_StackContent);
      }
      var stackTraceNode = document.pgs_getSingleNodeWithXPath(stackTraceLabelXPath + '/../following-sibling::div'); // save before renaming button
      stackTraceLabel.setAttribute("class", "button");
      stackTraceLabel.innerText = "Copy Stack Trace";
      stackTraceLabel.onclick = function () {
        stackTraceLabel.setAttribute("style", "opacity: 0.1; transition: opacity 0.8s;");
        setTimeout(function () {
          stackTraceLabel.style.opacity = 1;
        }, fadeInMs);
        GM_setClipboard(stackTraceNode.innerText);
      };
      document.pgs_StackContent = document.pgs_getSingleNodeWithXPath('//div[@class="content"]');
      document.pgs_StackContent.setAttribute('class', 'stack_content');

      document.pgs_StackContent.parentNode.appendChild(document.createElement('p'));

      var newContent = document.createElement('div');
      newContent.setAttribute('class', 'content');
      document.pgs_StackContent.insertAdjacentElement("afterend", newContent);
      //document.pgs_getSingleNodeWithXPath('//li[text()="Logs"]').click()
    }
  };

  // after every click (e.g. on the Prev or Next button) redraw the copy button
  document.onclick = function () {
    // We can't attempt this change until AJAX finishes rendering the
    // screen content, so as a hack we'll pause a bit before trying
    setTimeout(makeStackIntoButton, 500);
  };

  // don't require an additional click to initially display the button
  document.head.click();
})();
