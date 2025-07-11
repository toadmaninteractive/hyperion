@echo off
setlocal enableextensions enabledelayedexpansion

set out_dir="..\apps"

bin\igorc.exe -d -v -erl ^
  -p "igor\common" ^
  -p "igor\db" ^
  -o %out_dir%\db ^
  *.igor

if errorlevel 1 pause && exit

bin\igorc.exe -d -v -erl ^
  -i "igor\common\*.igor" ^
  -i "igor\db\*.igor" ^
  -o %out_dir%\web ^
  igor\web\*.igor

if errorlevel 1 pause

endlocal
