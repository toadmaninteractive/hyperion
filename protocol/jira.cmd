@echo off
setlocal enableextensions enabledelayedexpansion

set out_dir="..\apps\jira"

bin\igorc.exe -d -v -erlang ^
  -p "igor\jira" ^
  -o %out_dir% ^
  *.igor

if errorlevel 1 pause

endlocal
