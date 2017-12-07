// ==UserScript==
// @name         Redirect URLs in HEAD of smoketest screenshot to localhost
// @namespace    tag:paul.schaaf@gmail.com,2010-01-01
// @version      2017.12.06
// @author       P.G. Schaaf
// @match        http://thfiles/autofiles/results/*html
// @require      file:///home/pschaaf/src/javascript/greasemonkey/Redirect URLs in HEAD of smoketest screenshot to localhost.user.js
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

/*

fail:
th-lin-064.guidewire.com:16625/cc/resources/css/font.css Failed to load resource: net::ERR_CONNECTION_REFUSED
th-lin-064.guidewire.com:16625/cc/resources/css/gen/pretheme.css Failed to load resource: net::ERR_CONNECTION_REFUSED
th-lin-064.guidewire.com:16625/cc/resources/css/gen/gw-theme--default.css Failed to load resource: net::ERR_CONNECTION_REFUSED
th-lin-064.guidewire.com:16625/cc/resources/css/app.css Failed to load resource: net::ERR_CONNECTION_REFUSED
th-lin-064.guidewire.com:16625/cc/resources/js/gen/all.js Failed to load resource: net::ERR_CONNECTION_REFUSED
icon_claimOpen_infobar.png Failed to load resource: net::ERR_CONNECTION_REFUSED
infobar_cp.png Failed to load resource: net::ERR_CONNECTION_REFUSED
displaykeyvalues Failed to load resource: net::ERR_CONNECTION_REFUSED
menu_claim_search.png Failed to load resource: net::ERR_CONNECTION_REFUSED

success:

fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:11 GET http://th-lin-064.guidewire.com:16625/cc/resources/css/font.css net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:12 GET http://th-lin-064.guidewire.com:16625/cc/resources/css/gen/pretheme.css net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:13 GET http://th-lin-064.guidewire.com:16625/cc/resources/css/gen/gw-theme--default.css net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:14 GET http://th-lin-064.guidewire.com:16625/cc/resources/css/app.css net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:19 GET http://th-lin-064.guidewire.com:16625/cc/resources/js/gen/all.js net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:735 GET http://th-lin-064.guidewire.com:16625/cc/resources/img/app/icon_claimOpen_infobar.png net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:747 GET http://th-lin-064.guidewire.com:16625/cc/resources/img/app/infobar_cp.png net::ERR_CONNECTION_REFUSED
fail.gw.smoketest.cc.fnol.FNOLCommercialPackageLossTypeTest.testCommericalPackageChooseLiabilityResultsInLossTypeGL.html:1450 GET http://th-lin-064.guidewire.com:16625/cc/service/displaykeyvalues?checksum=rwf3q%2BJBHtwOr640XbuCbaO3D6w%3D net::ERR_CONNECTION_REFUSED


*/