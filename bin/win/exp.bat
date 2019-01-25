@echo off
set ARGS=%*
if "%ARGS%"=="" set ARGS=%CD%

explorer %ARGS%
