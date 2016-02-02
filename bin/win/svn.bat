@echo off
REM Replace any backslashes with forward slashes
echo %* | sed 's/\\\\/\//g' | xargs svn