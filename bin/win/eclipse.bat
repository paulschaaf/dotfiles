@echo off
set JAVA_HOME=%JAVA_HOME_1.7%
set PATH=%JAVA_HOME%\bin;%PATH%

cd c:\eclipse
start eclipse.exe
goto END

cd ScalaClass
start "Scala sbt" cmd /T:5f /K echo submit paul.schaaf@gmail.com bfMrdCXZ3Z

REM java -version
REM path

cd ..

:END