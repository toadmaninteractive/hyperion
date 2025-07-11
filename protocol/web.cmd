@echo off
setlocal enableextensions enabledelayedexpansion

set out_dir="..\web\frontend\src\app\protocol"

bin\igorc.exe -v -t ts ^
  -x "gen_ts\*.cs" ^
  -p "igor\common" ^
  -p "igor\db" ^
  -p "igor\web" ^
  -o %out_dir% ^
  *.igor

if errorlevel 1 pause && exit

copy /B /V /Y "ts\igor.ts" %out_dir%

if errorlevel 1 pause

endlocal
