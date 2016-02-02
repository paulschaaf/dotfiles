@echo off

if "%1"=="" goto usage
if "%1"=="~" goto end
	call image %0 %1

	REM recurse against remaining arguments
	%0 %2 %3 %4 %5 %6 %7 %8 %9 ~

:usage
echo Usage: Arguments can be any of: linux win2k dos622 win2kos games boot swap share apps data root home
echo 

:end
