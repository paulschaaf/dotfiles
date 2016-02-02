@echo off
REM Redirect to the shell script
@cygpath -a %* | xargs -d '\n' bash ~/bin/win/emacs
