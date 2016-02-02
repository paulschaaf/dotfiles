REM Spawn high-priority cmd titled "%*" in ".\bin" dir, white text on blue, executing %*
start "%*" /D bin /HIGH cmd /T:1F /K %*
