javascript:

url=location.href;

if( (urlL = prompt('Left%20URL', url)) && (urlR = prompt('Right%20URL', urlL)) ) {

  with (document) {
    write('<html><body%20onload="q=String.fromCharCode(34);f=window.frames[\'urlF\'].document;f.open();f.write(\'<html><body%20style=\\\'padding:0;margin:0\\\'><table%20width=\\\'100%\\\'%20cellspacing=0%20cellpadding=0><tr><td%20width=\\\'50%\\\'><input%20id=urlL%20onkeydown=\\\'if(window.event.keyCode==13)parent.leftF.location.href=this.value\\\'%20style=\\\'width:100%\\\'%20type=\\\'text\\\'%20value=\\\''
      + urlL
      + '\\\'/></td><td%20width=\\\'0\\\'><button%20onclick=\\\'parent.leftF.location.href=document.getElementById(\'+q+\'urlL\'+q+\').value\\\'>Go</button></td><td%20width=\\\'50%\\\'><input%20id=urlR%20onkeydown=\\\'if(window.event.keyCode==13)parent.rightF.location.href=this.value\\\'%20style=\\\'width:100%\\\'%20type=\\\'text\\\'%20value=\\\''
      + urlL
      + '\\\'/></td><td%20width=\\\'0\\\'><button%20onclick=\\\'parent.rightF.location.href=document.getElementById(\'+q+\'urlR\'+q+\').value\\\'>Go</button></td></tr></table></body></html>\');f.close();"><frameset%20rows="21px,*"><frame%20name="urlF"%20src=""%20noresize%20scrolling="no"></frame><frameset%20cols="*,*"><frame%20onload="if(window.frames.leftF.location.href!=undefined)window.frames.urlF.document.getElementById(\'urlL\').value=window.frames.leftF.location.href;"%20name="leftF"%20src="'
      + urlR
      + '"/><frame%20onload="if(window.frames.rightF.location.href!=undefined)window.frames.urlF.document.getElementById(\'urlR\').value=window.frames.rightF.location.href;"%20name="rightF"%20src="'
      + urlR
      + '"/></frameset></frameset></body></html>');
    void(close());
  }

}

else {
  void(null);
};


/*
<html>
<body%20onload="q=String.fromCharCode(34);f=window.frames[\'urlF\'].document;f.open();f.write(\'<html>
<body%20style=\\'padding:0;margin:0\\'>
<table%20width=\\'100%\\'%20cellspacing=0%20cellpadding=0>
<tr>
<td%20width=\\'50%\\'>
<input%20id=urlL%20onkeydown=\\'if(window.event.keyCode==13)parent.leftF.location.href=this.value\\'%20style=\\'width:100%\\'%20type=\\'text\\'%20value=\\''
      + #{urlL}
      + '\\'/>
</td>
<td%20width=\\'0\\'>
<button%20onclick=\\'parent.leftF.location.href=document.getElementById(\'+q+\'urlL\'+q+\').value\\'>
Go</button>
</td>
<td%20width=\\'50%\\'>
<input%20id=urlR%20onkeydown=\\'if(window.event.keyCode==13)parent.rightF.location.href=this.value\\'%20style=\\'width:100%\\'%20type=\\'text\\'%20value=\\''
      + #{urlL}
      + '\\'/>
</td>
<td%20width=\\'0\\'>
<button%20onclick=\\'parent.rightF.location.href=document.getElementById(\'+q+\'urlR\'+q+\').value\\'>
Go</button>
</td>
</tr>
</table>
</body>
</html>
\');f.close();">
<frameset%20rows="21px,*">
<frame%20name="urlF"%20src=""%20noresize%20scrolling="no">
</frame>
<frameset%20cols="*,*">
<frame%20onload="if(window.frames.leftF.location.href!=undefined)window.frames.urlF.document.getElementById(\'urlL\').value=window.frames.leftF.location.href;"%20name="leftF"%20src="'
      + #{urlL}
      + '"/>
<frame%20onload="if(window.frames.rightF.location.href!=undefined)window.frames.urlF.document.getElementById(\'urlR\').value=window.frames.rightF.location.href;"%20name="rightF"%20src="'
      + #{urlR}
      + '"/>
</frameset>
</frameset>
</body>
</html>
*/