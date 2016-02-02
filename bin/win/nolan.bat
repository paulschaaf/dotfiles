@echo off

if "%1"=="renew" goto lan

:nolan
   ipconfig /release "*LAN*"
   if "%2"=="renew" goto lan
   goto end

:lan
   ipconfig /renew   "*LAN*"
   goto end

:end
