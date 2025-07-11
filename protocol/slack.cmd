@echo off
setlocal enableextensions enabledelayedexpansion

set out_dir="..\apps\slack"

bin\igorc.exe -d -v -erlang ^
  -p "igor\slack" ^
  -o %out_dir% ^
  *.igor

if errorlevel 1 pause

endlocal
