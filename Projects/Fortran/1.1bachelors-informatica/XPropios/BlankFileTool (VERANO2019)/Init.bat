@echo off
REM Mod tool for Imperator:Rome, by SP. INSTRUCTIONS: DECOMPRESS ON THE FOLDER WHERE THE FILES ARE LOCATED, DOUBLE CLICK ON INIT.BAT AND YOU WILL HAVE YOUR FILES ON THE NEW FOLDER CREATED
REM Programs used: VsCode(Fortran95), only for windows
md ilovejohan
dir /b > ilovejohan\print.txt
start execute.exe
TIMEOUT /T 2
del /s ilovejohan\print.txt