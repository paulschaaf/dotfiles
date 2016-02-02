@echo off
if exist ghost\ghostpe.exe goto ghostpe
goto ghostee

:ghostpe
set exec=ghost\ghostpe
set args=
goto operation

:ghostee
set exec=ghost\ghost
set args=-auto -sure -batch
goto operation

:operation
if "%1"=="load" goto load
if "%1"=="save" goto save

echo Usage: %0 {"load" or "save"} file disk:partition
echo Save or load specified partion to ghost file. Save will use maximum
compression, and split the file into CD/RW-sized pieces.
goto end

:load
   echo **** Loading partition %3 from "%2" ....
   %exec% -clone,mode=pload,src=%2.gho:1,dst=%3 %args%
   goto end

:save
   echo **** Saving partition %3 to "%2" ....
   %exec% -clone,mode=pdump,src=%3,dst=%2.gho -split=630 -z9 %args%
   goto end

:end
set exec=
set args=
echo ÿ

