@echo off
rem P.G. Schaaf 2007-05-21

title RunAs:  Run a command under another user's credentials.
set user=pschaaf
set domain=@centripital
set command=c:\cygwin\bin\rxvt.exe -name xscreen -e /usr/bin/screen -R -DD

set /P user="RunAs which userid [%user%]: "
set /P user="User domain [%domain%]: "
set /P command="Command line to run [%command%]: "

echo c:\windows\system32\runas.exe /user:%user%%domain% \"%command%\"
rem pause
c:\windows\system32\runas.exe /user:%user%%domain% "%command%"
