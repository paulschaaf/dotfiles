javascript:(
   function() {
   	var   aWindow = window,
            aDocument = document,
   		   uri = encodeURIComponent,
   		   openWindow = aWindow.open(
   			   "http://www.google.com/bookmarks/mark?op=edit&output=popup&bkmk="
   				   + uri(aDocument.location)
      				+ "&title="
      				+ uri(aDocument.title),
      			"bkmk_popup",
      			"left="
      				+ ((aWindow.screenX||aWindow.screenLeft)+10)
      				+ ",top="
      				+ ((aWindow.screenY||aWindow.screenTop)+10)
      				+ ",height=420px,width=550px,resizable=1,alwaysRaised=1"
      		);
   	aWindow.setTimeout( function() {openWindow.focus()},300 )
   }
)();