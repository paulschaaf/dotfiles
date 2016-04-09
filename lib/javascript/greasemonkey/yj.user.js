// ==UserScript==
// @name           YJ Show video link
// @namespace      tag:paul.schaaf@gmail.com,2009-03-07
// @description    Show the URL of embedded videos
// @include        http://www.youjizz.com/videos/*
// ==/UserScript==

// version 1.0
// 2009/09/29
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// -------------------------------------------------------------------------
//
// This is a Greasemonkey user script.
//
// To install, you need Greasemonkey: http://greasemonkey.mozdev.org/
// Then restart Firefox and revisit this script.
// Under Tools, there will be a new menu item to "Install User Script".
// Accept the default configuration and install.
//
// To uninstall, go to Tools/Manage User Scripts, select this script,
// and click Uninstall.
//
// -------------------------------------------------------------------------

function PGS_mod () {
   var debugLevel = 2;
   var browser;

   if (navigator.appName.match(/Microsoft/))
     browser = 'msie';
   else if (navigator.vendor.match(/Google/))
   browser = 'chrome';
   else {
     browser = 'firefox';
   }

   Array.prototype.swap = function(x,y) {
     var tmp = this[x];
     this[x] = this[y];
     this[y] = tmp;
     return this;
   };

   // ===========================================================
   function DebugInfoHeader() {
     this.text = '';
   }

   DebugInfoHeader.prototype.length = function() {
     return this.text.length;
   };

   DebugInfoHeader.prototype.add = function(message) {
     this.text += message + '<br/>';
   };

   DebugInfoHeader.prototype.addValue = function(name, value) {
     this.add(name + ' == ' + value);
   };

   DebugInfoHeader.prototype.addProperties = function(name, object) {
     if (object == null) {
       this.addValue(name, object);
     }
     else {
       // not sure how to do this correctly in firefox
       for(prop in object) {
         this.addValue(name + '[' + prop + ']', object[prop]);
       }
     }
   };

   DebugInfoHeader.prototype.show = function() {
     if (debugLevel > 1 || (debugLevel == 1 && debugInfo.length() > 0)) {
       var debugElem = document.createElement('h1');
       var html = '<div id="PGS_debug" style="'
         + 'margin: 0 auto 0 auto;     border-bottom: 1px solid #000000;'
         + 'margin-bottom: 5px;        font-size: large;'
         + 'background-color: yellow;  color: black;'
         + 'text-align: left;'
         + '">'
         + '<u>DEBUG INFO</u> (debugLevel = ' + debugLevel
         + ', browser = ' + browser + ')<br/>'
         + '<p style="margin: 2px 0 1px 0; font-size: small;">';

       if (debugLevel > 2) {
         // Show browser properties
         this.addValue('navigator.appName', navigator.appName);
         this.addValue('navigator.appCodeName', navigator.appCodeName);
         this.addValue('navigator.appVersion', navigator.appVersion);
         this.addValue('navigator.product', navigator.product);
         this.addValue('navigator.vendor', navigator.vendor);
         if (debugLevel > 3) {
           this.addProperties('navigator', navigator);
         }
       }

       debugElem.innerHTML = html
         + this.text
         + '</p>'
         + '</div>';

       if (browser == 'msie') {
         document.insertBefore(debugElem, document.firstChild);
       }
       else {
         document.body.insertBefore(debugElem, document.body.firstChild);
       }
     };
   };

   var debugInfo = new DebugInfoHeader();

   // ===========================================================
   window.setTimeout(function() { debugInfo.show(); });

   // if (document.location.href.indexOf('/videos/') > 0) {
   var player        = document.getElementById('player');
   var link_code     = player.children[1].text;
   var html;
   var url;

   // debugInfo.addValue('link_code', link_code);

   if (so != null) {
     url = so.getVariable('file') || so.getVariable('content_video');
     debugInfo.add('Site URL Encoding: direct from "so"');
     debugInfo.addValue('so.variables', so.variables);
   }
   else if(link_code.match(/"so.addVariable(/)) {
     /* normal link_code is
      so.addVariable(
         "content_video",
         "http://media19.youjizz.com/xRzkEMEkoQ4NteS5f8KvOQ/SC16693.flv");
      */
     url = link_code.match(/http:.*flv/)[0];
     debugInfo.add('URL Encoding: content_video');
   }
   else if (link_code.match(/new SWFObject/)) {
     /* mangled URL looks like this:
      var so = new SWFObject(
                  "/youjizz_player/xRzkEMEkoQ4NteS5f8KvOQ/media1/brandienzo3.flv/player.swf",
                  "xmoov-flv-player","704","550","9");
      var so = new SWFObject(
                  "/youjizz_player/RPU17cU20QzlHSXkr3DBA/media18/SC42118.flv/player.swf",
                  "xmoov-flv-player","704","550","9");
      */
     var mangled_url = link_code.match(/youjizz_player\/(.*\.flv)/)[1];
     var url_pieces  = mangled_url.split('/');
     url_pieces.swap(0,1);
     url_pieces[0] += '.youjizz.com';
     url_pieces.unshift('http:/');
     debugInfo.addValue('mangled_url', mangled_url);
     debugInfo.addValue('url_pieces', url_pieces);
     url = url_pieces.join('/');
     debugInfo.add('Site URL Encoding: content_video');
   }
   else {
     url = null;
   }

   //var searchUrl = http://www.youjizz.com/search/Attractive--1.html
   var searchKeys = Array(
     'alexis-silver--1',
     'amateur-1',
     'asian--1',
     'attractive--1',
     'big--1',
     'blonde--1',
     'blowjob--1',
     'brunette-1',
     'club--1',
     'compilation--1',
     'cum-eating-1',
     'cumshot--1',
     'dark-hair--1',
     'deepthroat--1',
     'ebony--1',
     'fingerfuck--1',
     'fingering--1',
     'gianna-michaels-1',
     'group--1',
     'handjob--1',
     'hardcore--1',
     'jizz-1',
     'latina--1',
     'lily--1',
     'mature--1',
     'nigerian--1',
     'oral--1',
     'orgasm-1',
     'orgy--1',
     'party--1',
     'petite--1',
     'pov--1',
     'pussy-eating--1',
     'pussy-licking--1',
     'skinny--1',
     'squirt--1',
     'swallow-1',
     'teen--1',
     'tittie-fuck--1'
   );

   if (url) {
     debugInfo.addValue('url', url);
     html = '<a href="' + url + '">' + url + '</a>';
   }
   else {
     html = 'Cannot extract mangled URL.';
     if (debugLevel == 0) { debugLevel = 1; }
   }

   var newElem = document.createElement("a");
   newElem.innerHTML = '<h1>' + html + '</h1>';

   debugInfo.addValue('html', html);

   // remove the video
   //player.parentElement.removeChild(player);

   var ggrand_parent = document.getElementById('main');

   if (ggrand_parent == null) {
     var grand_parent  = player.parentElement.parentElement;
     ggrand_parent = grand_parent.parentElement;
   }

   ggrand_parent.insertBefore(newElem, ggrand_parent.children[0]);
};

PGS_mod();
